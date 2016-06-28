module Main
    ( main
    ) where

import Control.Monad
import System.Environment
import Text.Show.Pretty

import Data.Aviation.WX.Fetcher

main :: IO ()
main = do
    stations <- getArgs
    forM_ stations $ \station -> do
        putStrLn $ "Fetching weather information for " ++ station
        wx <- fetchMetar station
        putStrLn $ case wx of
            Right weather ->
                ppShow weather
            Left error ->
                "No information available for " ++ station ++ ": " ++ show error
