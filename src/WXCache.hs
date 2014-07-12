module Main where
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Network
import Network.HTTP
import System.Environment
import System.IO
import Text.Regex

main = withSocketsDo $ do
  -- Do this at the very beginning
  -- so we can easily bail out if the socket
  -- is already in use.
  sock <- listenOn $ PortNumber 13577
  
  args <- getArgs
  when (length args < 2) $ do
    putStrLn "Usage: ./WXCache ICAO 27,57"
    putStrLn "    Where ICAO is the identifier of the WX station"
    putStrLn "    and 27,57 says to update each 27th and 57th minute."
    putStrLn "Use 'nc 127.0.0.1 13577' to get the current WX."
    error "usage"
  let (wxstation:s_updateat) = args
  let updateat = parse $ unwords s_updateat
  curwx <- atomically $ newTVar ""
  updatewx wxstation curwx
  
  cwx <- atomically $ readTVar curwx
  putStrLn $ "Cur WX: " ++ cwx
  
  forkIO $ manageupdates (updatewx wxstation curwx) updateat
  putStrLn "Update thread started."
  
  putStrLn $ "Listening on port 13577."
  sequence_ . repeat $ do
    (h,_,_) <- accept sock
    forkIO $ do
      cwx <- atomically $ readTVar curwx
      hPutStrLn h cwx
      hFlush h
      hClose h
  
re_updateat :: Regex
re_updateat = mkRegex "([0-5][0-9])+,? *"
parse :: String -> [Int]
parse updateat = case matchRegexAll re_updateat updateat of
  Just (_, _, _, vals) -> read <$> vals
  Nothing -> []
  
formatTimeX :: FormatTime t => t -> Int
formatTimeX = read . formatTime defaultTimeLocale "%M"
  
manageupdates :: IO () -> [Int] -> IO ()
manageupdates fun whentoupdate = sequence_ . repeat $ do
  ct <- formatTimeX <$> getCurrentTime
  putStrLn $ show ct
  when ((== ct) `any` whentoupdate) $ do
    putStrLn "Updating now!"
    fun
  threadDelay 60000000

re_validicao :: Regex
re_validicao = mkRegex "^([A-Z][A-Z][A-Z][A-Z])$"

matchwx :: String -> Maybe String
matchwx wx = case res of
  Just [] -> Nothing
  Just s -> Just (s !! 0)
  Nothing -> Nothing
  where res = matchRegex re_validicao wx
  
updatewx :: String -> TVar String -> IO ()
updatewx wxicao tv = do
  case matchwx wxicao of
    Just icao -> do
      wx <- simpleHTTP (getRequest $ "http://weather.noaa.gov/pub/data/observations/metar/stations/" ++ icao ++ ".TXT") >>= getResponseBody
      let wxline = last . lines $ wx
      atomically $ writeTVar tv wxline
    Nothing -> atomically $ writeTVar tv "Illegal ICAO identifier"
