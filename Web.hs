{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
module Main where

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.Default (def)
import Data.Monoid (mempty)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Numeric
import Strive hiding (map, error)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B
import qualified Data.Cache.LRU.IO as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
    ("GET", ["segments.kml"]) ->
        oAuth def (url ++ "segments.kml") (segmentsKml "ride") (queryString req) >>= respond
    ("GET", ["segments-run.kml"]) ->
        oAuth def (url ++ "segments-run.kml") (segmentsKml "run") (queryString req) >>= respond
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
                Left (_, err) -> return $ responseBadReq err
                Right res' -> return $ f $ T.unpack $ get accessToken res'
        _ ->
            return $ responseBadReq "missing code"

segmentsKml typ token = responseKml $ K.netLinkKML "Strava Segments" href format
    where
        href = url ++ "segmentsView.kml"
        format = "south=[bboxSouth]&west=[bboxWest]&north=[bboxNorth]" ++
            "&east=[bboxEast]&token=" ++ token ++ "&type=" ++ typ

segmentsViewKml stravaLru =
    getParamsM [["south"], ["west"], ["north"], ["east"], ["token"], ["type", "ride"]] $
        \[s, w, n, e, token, typ] -> fmap (responseKml . segsToKml) $
            exploreBbox stravaLru (Just $ T.decodeUtf8 token) (rd s, rd w, rd n, rd e) (act typ)
    where
        rd = read . B.unpack
        act t = case t of "ride" -> Riding; "run" -> Running; _ -> error "unknown activity type"

exploreBbox stravaLru token bbox act = do
    client <- getClient stravaLru token
    Right segs <- exploreSegments client bbox $ set activityType act def
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

getParamsM :: (Monad m) => [[B.ByteString]] -> ([B.ByteString] -> m Response) -> Query -> m Response
getParamsM ps c q =
    case sequence $ map getPar ps of
        Right ps' -> c ps'
        Left err -> return $ responseBadReq err
    where
        getPar [] = error "getPar []"
        getPar (p:ds) =
            case lookup p q of
                Just (Just v) -> Right v
                _ -> case ds of
                    [] -> Left $ "missing param: " ++ B.unpack p
                    [d] -> Right d
                    _ -> error "getPar [,,]"
