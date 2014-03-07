{-# LANGUAGE OverloadedStrings #-}

module WX (
  parseWeather, weatherParser,

  Weather
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Maybe
import           Data.Text            (Text, pack)

-- For debugging :-)
-- import           System.IO.Unsafe

data Weather = METAR {
  date        :: Date,
  station     :: Station,
  flags       :: [Flag],
  wind        :: Wind,
  visibility  :: [Visibility],
  runwayvis   :: [(Runway, [Visibility], VisTrend)],
  runwaycond  :: [RunwayCondition],
  wx          :: [WeatherPhenomenon],
  clouds      :: [Cloud],
  pressure    :: Pressure,
  temperature :: Int,
  dewPoint    :: Int,
  trend       :: Trend,
  remark      :: Maybe Text
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

data Trend = BECMG [Transition]
           | TEMPO [Transition]
           | PROB Int Trend
           | NOSIG
           | NOTAVAIL
           deriving (Eq, Show)

data Transition = TransWind Wind | TransVis [Visibility] |
                  TransRunwayVis [(Runway, [Visibility], VisTrend)]  |
                  TransWX [WeatherPhenomenon] | TransClouds [Cloud]
        deriving (Eq, Show)

data VisTrend = VisTrendUpward | VisTrendDownward | VisTrendNoDistinctTendency
        deriving (Eq, Show)

data Pressure = QNH Int
              | MMHG Int
              | QFE Int
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

data Distance = Metres Int | KM Int | SM Int | NM Int
        deriving (Eq, Show)

data Visibility = TenOrMore
                | FiftyMetresOrLess
                | TwoOrMore
                | Visibility Distance (Maybe Direction) deriving (Eq, Show)

data Direction = North | South | East | West | NorthWest | NorthEast | SouthWest | SouthEast |
                 RWYLeft  | RWYRight | RWYCenter
        deriving (Eq, Show)

data Runway = AllRunways | Runway Int (Maybe Direction)
        deriving (Eq, Show)

data RwyCoverType = RCTDry | RCTMoist | RCTWet | RCTRime | RCTDrySnow | RCTWetSnow |
                    RCTSlush | RCTIce | RCTFZRut | RCTUnknown
        deriving (Eq, Show, Enum)
        
data RunwayCondition = RunwayCondition {
        rwcondRunway :: Runway,
        rwcondCover  :: RwyCoverType,
        rwcondSpread :: Maybe Int,
        rwcondCoverHeight :: Maybe Int,
        rwcondBrkCoeff :: RunwayBraking } | RwyClosed | ADClosed
        deriving (Eq, Show)

data RunwayBraking = BrakingFriction Int | BrakingEffect Int
        deriving (Eq, Show)
        
data Date = Date Int Int Int deriving (Eq, Show)

data Station = ICAO Text deriving (Eq, Show)

data Vertical = Height Int
              | Altitude Int
              | FlightLevel Int
              deriving (Eq, Show)

data Wind = Wind {
  direction :: WindDirection,
  velocity  :: Unit,
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

data Unit = Knots Int | Miles Int | KMH Int
        deriving (Eq, Show)

data Cloud = VVis (Maybe Int) | Cloud Cover Vertical CloudType deriving (Eq, Show)

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
  return $ Wind dir2 (unit str) gustsies
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
  skipSpace
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

callsfor :: Text -> Parser a -> Parser a
a `callsfor` b = string a >> b

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
visibilityParser = skipSpace >> choice [tenormore, metres, arb, arb1]
  where
    tenormore = string "9999" >> return TenOrMore
    metres = (\a b c d dir -> Visibility (visunit $ read [a,b,c,d]) dir) <$> digit <*> digit <*> digit <*> digit <*> directionParser
    visunit :: Int -> Distance
    visunit n = if n > 5000
        then KM (n `quot` 1000)
        else Metres n
    arb  = (\a b unit -> Visibility (unit $ read [a,b])) <$> digit <*> digit <*> distanceUnitParser <*> directionParser
    arb1 = (\a unit -> Visibility (unit $ read ['0', a])) <$> digit <*> distanceUnitParser <*> directionParser

directionParser :: Parser (Maybe Direction)
directionParser = Nothing `option` (Just <$> choice [
                "NE" `means` NorthEast, "NW" `means` NorthWest,
                "SE" `means` SouthEast, "SW" `means` SouthWest,
                "N" `means` North, "S" `means` South,
                "E" `means` East, "W" `means` West])

distanceUnitParser :: Parser (Int -> Distance)
distanceUnitParser = choice ["KM" `means` KM, "SM" `means` SM, "NM" `means` NM]

cloudParser :: Parser [Cloud]
cloudParser = choice [(:[]) <$> vvisParser, nsc, cavok, clr, sepBy1' clds (char ' ')]
    where
      clds = do
        perhaps_ space
        intsy <- cloudIntensityParser
        height <- (\a b c -> Height $ (* 100) $ read [a, b, c]) <$> digit <*> digit <*> digit
        cloudType <- cloudTypeParser
        return $ Cloud intsy height cloudType
      cavok = skipSpace >> "CAVOK" `means` []
      nsc = skipSpace >> "NSC" `means` []
      clr = skipSpace >> "CLR" `means` []

vvisParser :: Parser Cloud
vvisParser = do
        _ <- string "VV"
        choice ["///" `means` VVis Nothing,
                (\a b c -> VVis . Just . read $ [a,b,c]) <$> digit <*> digit <*> digit]

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

runwayvisParser :: Parser (Runway, [Visibility], VisTrend)
runwayvisParser = do
        runway <- runwayDesignationParser
        _ <- char '/'
        vis <- choice ["M0050" `means` [FiftyMetresOrLess],
                       "P2000" `means` [TwoOrMore],
                       parseRwyVis]
        vistrend <- choice ["D" `means` VisTrendDownward,
                            "N" `means` VisTrendNoDistinctTendency,
                            "U" `means` VisTrendUpward]
        return (runway, vis, vistrend)
        where
                parseRwyVis :: Parser [Visibility]
                parseRwyVis = do
                        worstvis <- Nothing `option` (Just <$> choice [fourDigits, trieDigits] <*. "V")
                        vis <- Just <$> choice [fourDigits, trieDigits]
                        return [Visibility (Metres a) Nothing | Just a <- [worstvis, vis]]
                        
runwayconditionParser :: Parser RunwayCondition
runwayconditionParser = do
        runway <- runwayDesignationParser
        _ <- char '/'
        choice ["SNOCLO" `means` ADClosed,
                rwycond runway]
    where
        rwycond runway = do
                cover <- RCTUnknown `option` ((toEnum . read . (:[])) <$> digit)
                spread <- choice [char '/' >> return Nothing,
                                  (Just . read . (:[])) <$> digit]
                spreadheight <- choice [string "//" >> return Nothing,
                                       Just <$> tuhDigits]
                rkorbw <- tuhDigits
                let coff = if rkorbw <= 90
                        then BrakingFriction rkorbw
                        else BrakingEffect rkorbw
                return $ RunwayCondition runway cover spread spreadheight coff

fourDigits :: Parser Int
fourDigits = (\a b c d -> read [a,b,c,d]) <$> digit <*> digit <*> digit <*> digit

trieDigits :: Parser Int
trieDigits = (\a b c -> read [a,b,c]) <$> digit <*> digit <*> digit

tuhDigits :: Parser Int
tuhDigits = (\a b -> read [a,b]) <$> digit <*> digit

runwayDesignationParser :: Parser Runway
runwayDesignationParser = choice ["R88" `means` AllRunways, oneRunway]
  where
    oneRunway = do
        _ <- char 'R'
        magheading <- (\a b -> read [a,b]) <$> digit <*> digit :: Parser Int
        dir <- Nothing `option` (Just <$> choice ["L" `means` RWYLeft, "R" `means` RWYRight,
                                                  "C" `means` RWYCenter])
        return $ Runway magheading dir

trendParser :: Parser Trend
trendParser = choice ["NOSIG" `means` NOSIG, changeParser]
        where
                changeParser :: Parser Trend
                changeParser = do
                        changes <- changesParser :: Parser [Trend]
                        when (length changes /= 1) $ fail "A METAR contains exactly one transition"
                        return $ head changes

changesParser :: Parser [Trend]
changesParser = many1 $ skipSpace >> transitionTypeParser
        where
                transitionTypeParser :: Parser Trend
                transitionTypeParser = choice [
                        "TEMPO" `callsfor` (TEMPO <$> transitionParser),
                        "BECMG" `callsfor` (BECMG <$> transitionParser),
                        "PROB" `callsfor`  (PROB  <$> twoDigits <*> (head <$> changesParser))]
                transitionParser :: Parser [Transition]
                transitionParser = sepBy1' oneTransition (char ' ')
                oneTransition = do
                    skipSpace
                    choice . map try $
                      [
                       TransClouds    <$> cloudParser,
                       TransWind      <$> windParser,
                       TransVis       <$> many1' visibilityParser,
                       TransWX        <$> count 1 wxParser,
                       TransRunwayVis <$> sepBy' runwayvisParser (char ' ')
                      ]

twoDigits :: Parser Int
twoDigits = (\a b -> read [a,b]) <$> digit <*> digit

metarParser :: Parser Weather
metarParser = do
  _ <- string "METAR"
  reportflags <- flagsParser
  identifier <- skipSpace >> stationParser
  reportdate <- skipSpace >> dateParser
  reportwind <- skipSpace >> windParser
  skipSpace
  reportvis <- [TenOrMore] `option` many1 visibilityParser
  skipSpace
  reportrunwayvis <- sepBy' runwayvisParser (char ' ')
  reportrunwaycond <- sepBy' runwayconditionParser (char ' ')
  reportwx <- many' wxParser
  reportclouds <- skipSpace >> cloudParser
  (reporttemp, reportdewpoint) <- skipSpace >> tdParser
  reportpressure <- skipSpace >> pressureParser
  skipSpace
  reportrmk <- maybeRMK
  reporttrend <- NOTAVAIL `option` trendParser
  _ <- choice $ map char ['=', '$']
  return $ METAR reportdate identifier reportflags reportwind reportvis reportrunwayvis reportrunwaycond reportwx reportclouds reportpressure reporttemp reportdewpoint reporttrend reportrmk

maybeRMK :: Parser (Maybe Text)
maybeRMK = Nothing `option` do
        _ <- string "RMK"
        Just . pack <$> many1 (satisfy (notInClass "$="))

weatherParser :: Parser Weather
weatherParser = metarParser

maybeOneSpace :: Parser ()
maybeOneSpace = perhaps_ space

parseWeather :: Text -> Either String Weather
parseWeather = parseOnly weatherParser
-- parseWeather text = unsafePerformIO (parseTest weatherParser text) `seq` parseOnly weatherParser text
