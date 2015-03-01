module KML (tracksToKML, Track(..)) where

import Text.XML.Light

data Track = Track
    { name :: String
    , desc :: String
    , coord :: [(Double, Double)]
    } deriving Show

tracksToKML :: [Track] -> String
tracksToKML = showTopElement . kmlTop

kmlTop tracks = unode "kml" (Attr (unqual "xmlns") "http://www.opengis.net/kml/2.2", kmlDocument tracks)

kmlDocument tracks = unode "Document" $ kmlPreamble ++ map kmlPlacemark tracks

kmlPlacemark track = unode "Placemark"
    [ unode "name" $ name track
    , unode "description" $ desc track
    , unode "LineString" $ unode "coordinates" $ coords $ coord track
    ]

coords cs = unlines [ show lon ++ "," ++ show lat ++ ",0" | (lat, lon) <- cs]

kmlPreamble = []
