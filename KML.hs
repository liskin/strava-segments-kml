module KML
    ( tracksToKML
    , netLinkKML
    , Track(..)
    ) where

import Text.XML.Light

data Track = Track
    { name :: String
    , desc :: String
    , coord :: [(Double, Double)]
    } deriving Show

tracksToKML :: [Track] -> String
tracksToKML = showTopElement . kmlTop . map kmlPlacemark

kmlTop content = unode "kml" (xmlns, unode "Document" content)
    where
        xmlns = Attr (unqual "xmlns") "http://www.opengis.net/kml/2.2"

kmlPlacemark track = unode "Placemark"
    [ unode "name" $ name track
    , unode "description" $ desc track
    , unode "LineString" $ unode "coordinates" $ coords $ coord track
    ]

coords cs = unlines [ show lon ++ "," ++ show lat ++ ",0" | (lat, lon) <- cs]

netLinkKML :: String -> String -> String -> String
netLinkKML name href format = showTopElement $ kmlTop [nameTag, netLinkTag]
    where
        nameTag = unode "name" name
        netLinkTag = unode "NetworkLink" [nameTag, linkTag]
        linkTag = unode "Link" [hrefTag, viewFormatTag, viewRefreshModeTag]
        hrefTag = unode "href" href
        viewFormatTag = unode "viewFormat" format
        viewRefreshModeTag = unode "viewRefreshMode" "onStop"
