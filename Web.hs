{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.Monad (join)
import Data.Default (def)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Strive hiding (map)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified KML as K

main :: IO ()
main = do
    [port] <- getArgs
    run (read port) app

app :: Application
app req respond = case (requestMethod req, pathInfo req) of
    ("GET", ["segments.kml"]) -> segmentsKml (queryString req) >>= respond

segmentsKml q = do
    segs <- exploreBbox token bbox
    return $ responseBuilder status200 headers $ fromString $ segsToKML segs
    where
        Just [south, west, north, east, token] = join $ fmap sequence $ sequence
            [ lookup p q | p <- ["south", "west", "north", "east", "token"] ]
        rd = read . B.unpack
        bbox = (rd south, rd west, rd north, rd east) :: (Double, Double, Double, Double)
        headers = [("Content-Type", "application/vnd.google-earth.kml+xml")]

exploreBbox token bbox = do
    client <- buildClient $ B.unpack token
    Right segs <- exploreSegments client bbox def
    return segs

segToTrack seg = K.Track
    { K.name = T.unpack $ get name seg
    , K.desc = ""
    , K.coord = unPolyline $ get points seg }

segsToKML segs = K.tracksToKML $ map segToTrack $ get segments segs
