module Main where

import           WX

import           Control.Applicative ()
import           Control.Monad
import           Data.Char
import           Data.Text           (pack)
import           Network.HTTP
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ do
        putStrLn "Weather parser. Currently only METARs are supported."
        putStrLn "    usage: ./metar ICAO [ICAO [ICAO]]\n"
        putStrLn "Where ICAO is the four letter code of the AD you're interested in."
  let stations = map (map toUpper) args
  mapM_ (\icao -> fetchMetar icao >>= printMetar icao) stations

printMetar :: String -> Either String Weather -> IO ()
printMetar icao (Right metar)  = putStrLn $ icao ++ "\t" ++ show metar
printMetar icao (Left errmsg)  = putStrLn $ icao ++ "\tNOTAVAIL\t" ++ errmsg

fetchMetar :: String -> IO (Either String Weather)
fetchMetar icao = do
  metarString <- simpleHTTP (getRequest url) >>= getResponseBody
  return . parseWeather . pack $ "METAR " ++ relLine metarString
  where
        url = "http://weather.noaa.gov/pub/data/observations/metar/stations/" ++ icao ++ ".TXT"
        relLine s = Prelude.lines s !! 1
