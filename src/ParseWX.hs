module Main where

import WX
import Data.Text (pack)

main :: IO ()
main = do
        putStrLn "Enter METAR<ENTER>"
        interact $ unlines . (map $ show . parseWeather . pack) . lines
              