{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
module Main where

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.Default (def)
import Data.Monoid (mempty)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Numeric
import Strive hiding (map)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B
import qualified Data.Cache.LRU.IO as C
import qualified Data.Text as T
import qualified KML as K
import qualified Text.Html as H

import Config

main :: IO ()
main = do
    [port] <- getArgs
    stravaLru <- C.newAtomicLRU (Just 100)
    run (read port) $ logger $ app stravaLru

app stravaLru req respond = case (requestMethod req, pathInfo req) of
    ("GET", []) -> respond $ responseFile status200 [("Content-Type", "text/html")] "index.html" Nothing
    ("GET", ["segments.kml"]) -> oAuth def (url ++ "segments.kml") segmentsKml (queryString req) >>= respond
    ("GET", ["segmentsView.kml"]) -> segmentsViewKml stravaLru (queryString req) >>= respond
    ("GET", _) -> respond $ responseNotFound
    (_, _) -> respond $ responseNotImplemented

oAuth opts pageUrl f q =
    case lookup "code" q of
        Nothing ->
            return $ responseFound $ B.pack $ buildAuthorizeUrl clientId pageUrl opts
        Just (Just code) -> do
            res <- exchangeToken clientId clientSecret (B.unpack code)
            case res of
                Left err -> return $ responseBadReq err
                Right res' -> return $ f $ T.unpack $ get accessToken res'
        _ ->
            return $ responseBadReq "missing code"

segmentsKml token = responseKml $ K.netLinkKML "Strava Segments" href format
    where
        href = url ++ "segmentsView.kml"
        format = "south=[bboxSouth]&west=[bboxWest]&north=[bboxNorth]&east=[bboxEast]&token=" ++ token

segmentsViewKml stravaLru =
    getParamsM ["south", "west", "north", "east", "token"] $ \[s, w, n, e, token] ->
        fmap (responseKml . segsToKml) $ exploreBbox stravaLru (B.unpack token) (rd s, rd w, rd n, rd e)
    where
        rd = read . B.unpack

exploreBbox stravaLru token bbox = do
    client <- getClient stravaLru token
    Right segs <- exploreSegments client bbox def
    return segs

getClient stravaLru token = do
    v <- C.lookup token stravaLru
    case v of
        Just client -> return client
        Nothing -> do
            client <- buildClient token
            C.insert token client stravaLru
            return client

segToTrack seg =
    K.Track
        { K.name = T.unpack $ get name seg
        , K.desc = H.prettyHtml desc
        , K.coord = unPolyline $ get points seg }
    where
        fmt n x = showFFloat (Just n) x ""
        -- for some reason Strava Android app doesn't catch the link with
        -- https, hence just http
        link = "http://www.strava.com/segments/" ++ show (get Strive.id seg)
        category = get climbCategoryDesc seg
        elev = fmt 0 (get elevDifference seg) ++ " m"
        dist = fmt 1 (get distance seg / 1000) ++ " km"
        grad = fmt 1 (get avgGrade seg) ++ "%"
        desc =
            [ H.toHtml $ H.hotlink link [H.stringToHtml link], H.br, H.br
            , H.bold $ H.stringToHtml "Category: ", H.stringToHtml category, H.br
            , H.bold $ H.stringToHtml "Distance: ", H.stringToHtml dist, H.br
            , H.bold $ H.stringToHtml "Elev. difference: ", H.stringToHtml elev, H.br
            , H.bold $ H.stringToHtml "Avg. grade: ", H.stringToHtml grad, H.br
            ]

segsToKml segs = K.tracksToKML $ map segToTrack $ get segments segs

responseKml = responseBuilder status200 headers . fromString
    where
        headers = [("Content-Type", "application/vnd.google-earth.kml+xml")]

responseBadReq s = responseBuilder status400 headers $ fromString s
    where
        headers = [("Content-Type", "text/plain")]

responseNotFound = responseBuilder status404 headers $ fromString "not found"
    where
        headers = [("Content-Type", "text/plain")]

responseNotImplemented = responseBuilder status501 headers $ fromString "not implemented"
    where
        headers = [("Content-Type", "text/plain")]

responseFound uri = responseBuilder status302 headers mempty
    where
        headers = [("Content-Type", "text/plain"), ("Location", uri)]

getParamsM :: (Monad m) => [B.ByteString] -> ([B.ByteString] -> m Response) -> Query -> m Response
getParamsM ps c q =
    case sequence $ map getPar ps of
        Right ps' -> c ps'
        Left err -> return $ responseBadReq err
    where
        getPar p =
            case lookup p q of
                Just (Just v) -> Right v
                _ -> Left $ "missing param: " ++ B.unpack p
