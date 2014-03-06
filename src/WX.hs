{-# LANGUAGE OverloadedStrings #-}

module WX where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Maybe
import           Data.Text

data Weather = METAR {
  date        :: Date,
  station     :: Station,
  flags       :: [Flag],
  wind        :: Wind,
  visibility  :: Visibility,
  wx          :: WeatherPhenomena,
  clouds      :: CloudInfo,
  pressure    :: Pressure,
  temperature :: Int,
  dewPoint    :: Int,
  trend       :: Trend
  }
             | ATIS
             | SPECI
             | TAF
             | AIRMET
             | SIGMET
             | GAMET
             deriving (Eq, Show)

data Flag = COR | AMD | AUTO
        deriving (Eq, Show)

data Trend = BECMG Changes
           | TEMPO Changes
           | PROB Int Trend
           | NOSIG
           | NOTAVAIL
           deriving (Eq, Show)

data Changes = Changes deriving (Eq, Show)

data Pressure = QNH Int
              | MMHG Int
              | QFE Int
              deriving (Eq, Show)

data WeatherPhenomena = NSW
                      | SIGWX [WeatherPhenomenon]
                      deriving (Eq, Show)
data WeatherPhenomenon = Phenomenon {
  intensity :: WPIntensity,
  desc      :: Maybe WPDesc,
  prec      :: Maybe WPPrecipitation,
  obfus     :: Maybe WPObfuscation,
  other     :: Maybe WPOther
  }
                       deriving (Eq, Show)

data WPIntensity = Light | Moderate | Heavy | Vicinity | Recent
                 deriving (Enum, Eq, Show)

data WPDesc = Shallow | Patches | WXPartial | LowDrifting
            | Blowing | Shower | Thunderstorm | Freezing
            deriving (Enum, Eq, Ord, Show)

data WPPrecipitation = Drizzle | Rain | Snow | ShowGrains
                     | IceCrystals | IcePellets | Hail | SnowPellets
                     | UnknownPrecipitation
                     deriving (Enum, Eq, Ord, Show)

data WPObfuscation = Mist | Fog | Smoke | VolcanicAsh
                   | Dust | Sand | Haze
                   deriving (Enum, Eq, Ord, Show)

data WPOther = DustOrSandwhirls | Squalls | Tornado
             | Sandstorm | Duststorm
             deriving (Enum, Eq, Ord, Show)

data Visibility = TenOrMore
                | Meters Int deriving (Eq, Show)

data Date = Date Int Int Int deriving (Eq, Show)

data Station = ICAO Text deriving (Eq, Show)

data Vertical = Height Int
              | Altitude Int
              | FlightLevel Int
              deriving (Eq, Show)

data Wind = Wind {
  direction :: WindDirection,
  velocity  :: (Int, Unit),
  gusts     :: Maybe Int
  } deriving (Eq, Show)

data WindDirection = Variable
                   | Degrees Int
                   | Varying {
                       mean :: Int,
                       from :: Int,
                       to   :: Int
                     }
                   deriving (Eq, Show)

data Unit = Knots | Miles | KMH deriving (Eq, Show)

data CloudInfo = CAVOK
               | NoSignificantClouds
               | Clouds [Cloud]
               deriving (Eq, Show)

data Cloud = Cloud Cover Vertical CloudType deriving (Eq, Show)

data CloudType = Cumulonimbus
               | ToweringCumulus
               | Stratus
               | Cumulus
               | Stratocumulus
               | Altostratus
               | Altocumulus
               | Cirrostratus
               | Cirrus
               | Unclassified deriving (Eq, Show)

data Cover = FEW | SCT | BKN | OVC deriving (Enum, Eq, Ord, Show)

stationParser :: Parser Station
stationParser = ICAO <$> Data.Attoparsec.Text.take 4

dateParser :: Parser Date
dateParser = Date <$> twin <*> twin <*> (twin <*. "Z")
    where twin = (\a b -> read [a, b]) <$> digit <*> digit

variableWindParser :: WindDirection -> Parser WindDirection
variableWindParser (Degrees meanWind) = try $ do
  dir1 <- (\a b c -> read [a, b, c]) <$> digit <*> digit <*> digit
  _ <- char 'V'
  dir2 <- (\a b c -> read [a, b, c]) <$> digit <*> digit <*> digit
  return $ Varying meanWind dir1 dir2
variableWindParser _ = fail "Erroneous parameters"

windParser :: Parser Wind
windParser = do
  dir <- choice [readwinddir, variablewind]
  str <- readwindstr
  gustsies <- option Nothing readgusts
  unit <- readunit
  dir2 <- option dir $ char ' ' >> variableWindParser dir
  return $ Wind dir2 (str, unit) gustsies
    where
      variablewind = "VRB" `means` Variable
      readwinddir = (\a b c -> Degrees . read $ [a, b, c]) <$> digit <*> digit <*> digit
      readwindstr = (\a b -> read [a, b]) <$> digit <*> digit
      readunit = choice ["KT" `means` Knots,
                         "MPH" `means` Miles,
                         "KM" `means` KMH]
      readgusts = (\_ b c -> Just . read $ [b, c]) <$> char 'G' <*> digit <*> digit

pressureParser :: Parser Pressure
pressureParser = choice [qnh, mmhg]
    where
      qnh = (\_ a b c d -> QNH $ read [a, b, c, d]) <$> char 'Q' <*> digit <*> digit <*> digit <*> digit
      mmhg = (\_ a b c d -> MMHG $ read [a, b, c, d]) <$> char 'A' <*> digit <*> digit <*> digit <*> digit

wxParser :: Parser WeatherPhenomenon
wxParser = do
  perhaps_ space
  intsy <- intensityParser
  dsc <- perhaps descParser
  prc <- perhaps precipitationParser
  obfs <- perhaps obfuscationParser
  othr <- perhaps otherParser
  when ((== 0) . Prelude.length . Prelude.filter not $
        [isNothing dsc, isNothing prc,
         isNothing obfs, isNothing othr]) $ fail ""
  return $ Phenomenon intsy dsc prc obfs othr

perhaps :: Parser a -> Parser (Maybe a)
perhaps parser = option Nothing $ Just <$> parser

perhaps_ :: Parser a -> Parser ()
perhaps_ parser = void $ perhaps parser

means :: Text -> a -> Parser a
a `means` b = string a >> return b

means' :: Text -> a -> Parser a
a `means'` b = try $ skipSpace >> string a >> skipSpace >> return b

descParser :: Parser WPDesc
descParser = choice ["MI" `means` Shallow,
              "BC" `means` Patches,
              "PR" `means` WXPartial,
              "DR" `means` LowDrifting,
              "BL" `means` Blowing,
              "SH" `means` Shower,
              "TS" `means` Thunderstorm,
              "FZ" `means` Freezing
             ]

precipitationParser :: Parser WPPrecipitation
precipitationParser = choice [
                       "DZ" `means` Drizzle,
                       "RA" `means` Rain,
                       "SN" `means` Snow,
                       "SG" `means` ShowGrains,
                       "IC" `means` IceCrystals,
                       "PL" `means` IcePellets,
                       "GR" `means` Hail,
                       "GS" `means` SnowPellets,
                       "UP" `means` UnknownPrecipitation
                      ]

obfuscationParser :: Parser WPObfuscation
obfuscationParser = choice ["BR" `means` Mist,
                            "FG" `means` Fog,
                            "FU" `means` Smoke,
                            "VA" `means` VolcanicAsh,
                            "DU" `means` Dust,
                            "SA" `means` Sand,
                            "HZ" `means` Haze]

otherParser :: Parser WPOther
otherParser = choice ["PO" `means` DustOrSandwhirls,
                      "SQ" `means` Squalls,
                      "FC" `means` Tornado,
                      "SS" `means` Sandstorm,
                      "DS" `means` Duststorm]

intensityParser :: Parser WPIntensity
intensityParser = option Moderate $ choice [
                   char '-' >> return Light,
                   char '+' >> return Heavy,
                   "VC" `means` Vicinity,
                   "RE" `means` Recent]

visibilityParser :: Parser Visibility
visibilityParser = choice [tenormore, metres, arb]
  where
    tenormore = string "9999" >> return TenOrMore
    metres = (\a b c d -> Meters $ read [a, b, c, d]) <$> digit <*> digit <*> digit <*> digit
    arb = (\a b unit -> Meters $ factorMW unit $ read [a, b]) <$> digit <*> digit <*> choice [string "KM",
                                                                                              string "SM"]
factorMW :: Text -> Int -> Int
factorMW = factorMWS
factorMWS :: Text -> Int -> Int
factorMWS "KM" n = n
factorMWS "SM" n = round $ toRational n * 1.609344
factorMWS _ _    = undefined

cloudParser :: Parser CloudInfo
cloudParser = choice [ nsc, cavok, Clouds <$> many' clds]
    where
      clds = do
        perhaps_ space
        intsy <- cloudIntensityParser
        height <- (\a b c -> Height $ (* 100) $ read [a, b, c]) <$> digit <*> digit <*> digit
        cloudType <- cloudTypeParser
        return $ Cloud intsy height cloudType
      cavok = maybeOneSpace >> "CAVOK" `means` CAVOK
      nsc = maybeOneSpace >> "NSC" `means` NoSignificantClouds

cloudIntensityParser :: Parser Cover
cloudIntensityParser = choice ["FEW" `means` FEW,
                               "SCT" `means` SCT,
                               "BKN" `means` BKN,
                               "OVC" `means` OVC]

cloudTypeParser :: Parser CloudType
cloudTypeParser = option Unclassified $ choice ["CB" `means` Cumulonimbus,
                                                "TCU" `means` ToweringCumulus,
                                                "ST" `means` Stratus,
                                                "CU" `means` Cumulus,
                                                "SC" `means` Stratocumulus,
                                                "AS" `means` Altostratus,
                                                "AC" `means` Altocumulus,
                                                "CS" `means` Cirrostratus,
                                                "CI" `means` Cirrus]

perhapsMinus :: Parser String
perhapsMinus = "" `option` (char 'M' >> return "-")

tdParser :: Parser (Int, Int)
tdParser = do
  tmpr <- (\pm a b -> read (pm ++ [a, b]) :: Int) <$> perhapsMinus <*> digit <*> digit
  _ <- char '/'
  dewpoint <- (\pm a b -> read (pm ++ [a, b]) :: Int) <$> perhapsMinus <*> digit <*> digit
  return (tmpr, dewpoint)

flagsParser :: Parser [Flag]
flagsParser = many' $ choice ["COR" `means'` COR,
                              "AMD" `means'` AMD,
                              "AUTO" `means'` AUTO]

metarParser :: Parser Weather
metarParser = do
  _ <- string "METAR"
  reportflags <- flagsParser
  identifier <- skipSpace >> stationParser
  reportdate <- skipSpace >> dateParser
  reportwind <- skipSpace >> windParser
  skipSpace
  reportvis <- option TenOrMore visibilityParser
  reportwx <- checksigwx <$> many' wxParser
  reportclouds <- many' spc >> cloudParser
  (reporttemp, reportdewpoint) <- spc >> tdParser
  reportpressure <- spc >> pressureParser
  return $ METAR reportdate identifier reportflags reportwind reportvis reportwx reportclouds reportpressure reporttemp reportdewpoint reporttrend
    where
      spc = skipMany1 space
      checksigwx :: [WeatherPhenomenon] -> WeatherPhenomena
      checksigwx [] = NSW
      checksigwx reportwx = SIGWX reportwx
      reporttrend = NOTAVAIL


weatherParser :: Parser Weather
weatherParser = metarParser

maybeOneSpace :: Parser ()
maybeOneSpace = perhaps_ space

parseWeather :: Text -> Either String Weather
parseWeather = parseOnly weatherParser
