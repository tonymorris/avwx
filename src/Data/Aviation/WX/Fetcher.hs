{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.WX.Fetcher(
  parseWeather
, fetchMetar
) where

import Data.Attoparsec.Text(parseOnly)
import Data.Aviation.WX(Weather, weatherParser)
import Data.Char(toUpper, isAlpha)
import Data.List(filter)
import Data.Text(Text, pack)
import Network.HTTP(simpleHTTP, getRequest, getResponseBody)

type ICAOStationDesignator = String

-- | Parse the given METAR text.
parseWeather :: Text -> Either String Weather
parseWeather = parseOnly weatherParser

fetchMetar :: ICAOStationDesignator -> IO (Either String Weather)
fetchMetar icao = do
    let icao' = map toUpper . filter isAlpha $ icao
    metarString <- simpleHTTP (getRequest $ url icao') >>= getResponseBody
    let wx' = pack $ "METAR " ++ relLine metarString
    putStrLn $ "Parsing " ++ show wx'
    return $ parseWeather wx'
    where
        url designator = "http://tgftp.nws.noaa.gov/data/observations/metar/stations/" ++ designator ++ ".TXT"
        relLine s = Prelude.lines s !! 1
