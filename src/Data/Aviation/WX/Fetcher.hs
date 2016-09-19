{-# LANGUAGE OverloadedStrings #-}

module Data.Aviation.WX.Fetcher where

import Control.Lens
import Data.Char
import Data.List (filter, find)
import Data.Text (pack)
import Data.Text.Lens
import Network.HTTP
import Network.URI

import Data.Aviation.WX
import Data.Text(Text)
import Data.Attoparsec.Text(parseOnly)
import Text.Parsec(parse)
import Text.Parsec.Error(ParseError)
import Text.Parser.Char
import Text.Parser.Combinators
import Control.Monad.Trans.Either

type ICAOStationDesignator = String

-- | Parse the given METAR text.
parseWeather :: Text -> Either String Weather
parseWeather = parseOnly weatherParser

fetchMetar :: ICAOStationDesignator -> IO (Either String Weather)
fetchMetar icao = do
    let icao' = map toUpper . filter isAlpha $ icao
    metarString <- simpleHTTP (getRequest $ url icao') >>= getResponseBody
    let wx = pack $ "METAR " ++ relLine metarString
    putStrLn $ "Parsing " ++ show wx
    return $ parseWeather wx
    where
        url designator = "http://tgftp.nws.noaa.gov/data/observations/metar/stations/" ++ designator ++ ".TXT"
        relLine s = Prelude.lines s !! 1

data BomMetarState =
  NewSouthWales
  | Victoria
  | Queensland
  | WesternAustralia
  | SouthAustralia
  | Tasmania
  | NorthernTerritory
  deriving (Eq, Ord, Show)

stateFormParam ::
  Prism' String BomMetarState
stateFormParam =
  prism'
    (\s ->  case s of
              NewSouthWales ->
                "New-South-Wales"
              Victoria ->
                "Victoria"
              Queensland ->
                "Queensland"
              WesternAustralia ->
                "Western-Australia"
              SouthAustralia ->
                "South-Australia"
              Tasmania ->
                "Tasmania"
              NorthernTerritory ->
                "Northern-Territory")
    (\s ->  case s of
              "New-South-Wales" ->
                Just NewSouthWales
              "Victoria" ->
                Just Victoria
              "Queensland" ->
                Just Queensland
              "Western-Australia" ->
                Just WesternAustralia
              "South-Australia" ->
                Just SouthAustralia
              "Tasmania" ->
                Just Tasmania
              "Northern-Territory" ->
                Just NorthernTerritory
              _ -> 
                Nothing
              )

fetchBomRequest :: BomMetarState -> Request String
fetchBomRequest s =
  let p = "page=metar-speci&state=" ++ stateFormParam # s
  in  Request
        (URI "http:" (Just (URIAuth "" "www.bom.gov.au" "")) "/aviation/php/process.php" "" "")
        POST
        [ 
          mkHeader HdrContentLength (show (length p))
        , mkHeader HdrContentType "application/x-www-form-urlencoded"
        ]
        p

parseBomWeather :: (Monad f, CharParsing f) => f [Either String Weather]
parseBomWeather =
  let tryManyTill s = manyTill anyChar (try (string s))
  in  many $  do  try $ tryManyTill "<p class=\"product\">"
                  x <- tryManyTill "</p>"
                  return (parseWeather (pack x))

fetchBom :: BomMetarState -> EitherT ParseError IO [Either String Weather]
fetchBom s =
  EitherT $ do  r <- simpleHTTP (fetchBomRequest s)
                s <- getResponseBody r
                return (parse parseBomWeather "bom.gov.au" (pack s))

fetchBomMetar ::
  BomMetarState
  -> String
  -> EitherT ParseError IO (Maybe Weather)
fetchBomMetar s i =
  findOf (each . _Right) (\w -> w ^. station . _Wrapped . unpacked == i) <$> fetchBom s
