module KML
    ( tracksToKML
    , netLinkKML
    , Track(..)
    ) where

import Text.XML.Light
import Strive.Aliases (Latitude, Longitude)

data Track = Track
    { name :: String
    , desc :: String
    , coord :: [(Latitude, Longitude)]
    } deriving Show

tracksToKML :: [Track] -> String
tracksToKML = showTopElement . kmlTop . map kmlPlacemark

kmlTop :: Node t => t -> Element
kmlTop content = unode "kml" (xmlns, unode "Document" content)
    where
        xmlns = Attr (unqual "xmlns") "http://www.opengis.net/kml/2.2"

kmlPlacemark :: Track -> Element
kmlPlacemark track = unode "Placemark"
    [ unode "name" $ name track
    , unode "description" $ desc track
    , unode "LineString" $ unode "coordinates" $ coords $ coord track
    ]

coords :: [(Longitude, Latitude)] -> String
coords cs = unlines [ show lon ++ "," ++ show lat ++ ",0" | (lat, lon) <- cs]

netLinkKML :: String -> String -> String -> String
netLinkKML n href format = showTopElement $ kmlTop [nameTag, netLinkTag]
    where
        nameTag = unode "name" n
        netLinkTag = unode "NetworkLink" [nameTag, linkTag]
        linkTag = unode "Link" [hrefTag, viewFormatTag, viewRefreshModeTag]
        hrefTag = unode "href" href
        viewFormatTag = unode "viewFormat" format
        viewRefreshModeTag = unode "viewRefreshMode" "onStop"
