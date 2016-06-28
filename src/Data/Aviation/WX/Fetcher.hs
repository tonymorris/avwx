module Data.Aviation.WX.Fetcher where

import Data.Char
import Data.List (filter)
import Data.Text (pack)
import Network.HTTP

import Data.Aviation.WX

type ICAOStationDesignator = String

fetchMetar :: ICAOStationDesignator -> IO (Either String Weather)
fetchMetar icao = do
    let icao' = map toUpper . filter isAlpha $ icao
    metarString <- simpleHTTP (getRequest $ url icao') >>= getResponseBody
    let wx = pack $ "METAR " ++ relLine metarString ++ "="
    putStrLn $ "Parsing " ++ show wx
    return $ parseWeather wx
    where
        url designator = "http://weather.noaa.gov/pub/data/observations/metar/stations/" ++ designator ++ ".TXT"
        relLine s = Prelude.lines s !! 1
