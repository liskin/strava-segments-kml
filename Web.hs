{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.Monad (join)
import Control.Monad.Identity (Identity(..))
import Data.Default (def)
import Data.Monoid (mempty)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Strive hiding (map)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified KML as K

import Config

main :: IO ()
main = do
    [port] <- getArgs
    run (read port) $ logger app

app :: Application
app req respond = case (requestMethod req, pathInfo req) of
    ("GET", []) -> respond $ responseFile status200 [("Content-Type", "text/html")] "index.html" Nothing
    ("GET", ["segments.kml"]) -> segmentsKmlAuth (queryString req) >>= respond
    ("GET", ["segmentsView.kml"]) -> segmentsViewKml (queryString req) >>= respond
    ("GET", _) -> respond $ responseNotFound
    (_, _) -> respond $ responseNotImplemented

segmentsKmlAuth q =
    case lookup "code" q of
        Nothing ->
            return $ responseFound $ B.pack $ buildAuthorizeUrl clientId (url ++ "segments.kml") def
        Just (Just code) -> do
            res <- exchangeToken clientId clientSecret (B.unpack code)
            case res of
                Left err -> return $ responseBadReq err
                Right res' -> return $ segmentsKml $ T.unpack $ get accessToken res'

segmentsKml token = responseKml $ K.netLinkKML "Strava Segments" href format
    where
        href = url ++ "segmentsView.kml"
        format = "south=[bboxSouth]&west=[bboxWest]&north=[bboxNorth]&east=[bboxEast]&token=" ++ token

segmentsViewKml =
    getParamsM ["south", "west", "north", "east", "token"] $ \[s, w, n, e, token] ->
        fmap (responseKml . segsToKml) $ exploreBbox token (rd s, rd w, rd n, rd e)
    where
        rd = read . B.unpack

exploreBbox token bbox = do
    client <- buildClient $ B.unpack token
    Right segs <- exploreSegments client bbox def
    return segs

segToTrack seg = K.Track
    { K.name = T.unpack $ get name seg
    , K.desc = ""
    , K.coord = unPolyline $ get points seg }

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

responseFound url = responseBuilder status302 headers mempty
    where
        headers = [("Content-Type", "text/plain"), ("Location", url)]

getParams :: [B.ByteString] -> ([B.ByteString] -> Response) -> Query -> Response
getParams ps c q = runIdentity $ getParamsM ps (Identity . c) q

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
