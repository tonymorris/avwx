{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Data.Aviation.WX
-- Copyright: (C) 2014-2016, Hans-Christian Esperer
-- License: MIT
-- Maintainer: Hans-Christian Esperer <hc@hcesperer.org>
-- Stability: experimental
-- Portability: portable
--
-- Parse aviation weather reports. A qualified import is recommended.
module Data.Aviation.WX
    ( parseWeather
    , weatherParser
    , Weather(..)
    , Date(..)
    , Station(..)
    , Flag(..)
    , Wind(..)
    , Visibility(..)
    , Runway(..)
    , VisTrend(..)
    , RunwayCondition(..)
    , WeatherPhenomenon(..)
    , Cloud(..)
    , Pressure(..)
    , Trend(..)
    , WPDesc(..)
    , WPPrecipitation(..)
    , WPObfuscation(..)
    , WPOther(..)
    , Distance(..)
    , Direction(..)
    , RwyCoverType(..)
    , RunwayBraking(..)
    , Vertical(..)
    , WindDirection(..)
    , Cover(..)
    , CloudType(..)
    , WPIntensity(..)
    , Transition(..)
    , Unit(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Maybe
import Data.Text (Text, pack)

-- | Aviation weather, currently only METARs are supported.
data Weather
    = -- | A METeorological Aerodrome Report
      METAR
    { -- | The observation date.
      date        :: Date
    , -- | The designation of the observing station.
      station     :: Station
    , -- | A remark about the reported observation.
      flags       :: [Flag]
    , -- | The observed wind.
      wind        :: Maybe Wind
    , -- | The observed visibility.
      visibility  :: [Visibility]
    , -- | The observed visibility for specific runways,
      -- usually reported if the runway visibility significantly
      -- differs from the general reported visibility.
      runwayvis   :: [(Runway, [Visibility], Maybe VisTrend)]
    , -- | Surface or close conditions of a specific runway.
      runwaycond  :: [RunwayCondition]
    , -- | Observed weather phenomena
      wx          :: [WeatherPhenomenon]
    , -- | Observed cloud layers
      clouds      :: [Cloud]
    , -- | Measured pressure
      pressure    :: Maybe Pressure
    , -- | Measured pressure
      temperature :: Maybe Int
    , -- | Determined dew point
      dewPoint    :: Maybe Int
    , -- | Expected changes within the next two hours
      trend       :: Trend
    , -- | RMK section (Additional parts of a METAR report that are not
      -- part of the official METAR message but are commonly used
      -- in various parts of the world; unparsed)
      remark      :: Maybe Text }
    | -- | An automatic terminal information service report
      ATIS
    | -- | A non-scheduled METAR
      SPECI
    | -- | A terminal aerodrome forecast
      TAF
    | -- | An aviation wx hazard message of moderate severity
      AIRMET
    | -- | A significant meteorological information message
      SIGMET
    | -- | A general aviation forecase message
      GAMET
    deriving (Eq, Show)

-- | A flag describing an aviation meteorological report
data Flag
    = -- | A message has been corrected after the beginning of
      -- its original validity period
      COR
    | -- | A message has been corrected prior to its
      -- original validity period
      AMD
    | -- | A message has been generated fully automatic
      -- without a plausibility check by a human
      AUTO
    deriving (Eq, Show)

-- | The trend part of a METAR message specifies expected
-- changes in weather conditions within the next two hours.
-- A Trend/Transition part of a TAF message specified expected
-- changes in weather conditions within the specified range.
data Trend
    = -- | A transition that will start within the defined
      -- time frame and be completed at the end of the defined
      -- time frame
      BECMG [Transition]
    | -- | A transition that will start within the defined
      -- time frame and be finished at the end of the defined
      -- time frame
      TEMPO [Transition]
    | -- | A probability specification.
      -- As one of my FIs (ex-atc at EDDF) used to put it:
      -- 30% means "I'm quite sure it won't happen but will still
      -- put it in here, in case it does after all."
      -- 40% means "I'm certain it will happen but will still
      -- put it with 40%, in case it does not happen after all."
      PROB Int Trend
    | -- | NOSIG is only applicable to METARs. It means that
      -- no significant changes are expected within the next
      -- two hours.
      NOSIG
    | -- | NOTAVAIL is only applicable to METARs. It means that
      -- the METAR message in question does not contain a TREND
      -- section.
      NOTAVAIL
    deriving (Eq, Show)

-- | A transition in weather conditions.
-- A transition can either be temporary or permanent;
-- this will be encoded in the container structure.
data Transition
    = -- | A change of wind strength or direction
      TransWind Wind
    | -- | A change of visibility
      TransVis [Visibility]
    | -- | A change of visibility for a specific runway
      TransRunwayVis [(Runway, [Visibility], Maybe VisTrend)]
    | -- | A change of weather phenomenon
      TransWX [WeatherPhenomenon]
    | -- | A change of ceiling or cloud layers
      TransClouds [Cloud]
    deriving (Eq, Show)

-- | A visibility trend specifically for runway conditions
data VisTrend
    = -- | Visibility will improve (maybe do wait a bit)
      VisTrendUpward
    | -- | Visibility will deteriorate (still, don't rush
      -- the take off and use proper phraseology at all times)
      VisTrendDownward
    | -- | No expected change in runway visibility conditions
      VisTrendNoDistinctTendency
    deriving (Eq, Show)

-- | A pressure value. This is intentionally coded
-- individually and not converted to a specific reference.
data Pressure
    = -- | The QNH value in hectopascals. QNH is the current
      -- pressure at sea level, corrected for pressure and
      -- temperature changes at the station level.
      QNH Int
    | -- | The same as QNH, only in inches
      -- (Do you know the old joke?:
      --   * ATC: Liner 1723 descend to 3,000ft, the QNH is 1013mb.
      --   * Liner 1723: Uh, approach, can we have that in inches please?
      --   * ATC: Liner 1723 descend to 36,000 inches, the QNH is 1013mb.
      Altimeter Int
    | -- | The current pressure at station level in hectopascals.
      QFE Int
    | -- | The current pressure at sea level in hectopascals.
      QFF Int
    deriving (Eq, Show)

-- | A weather phenomenon.
-- This can be an observed phenomenon in the case of METARs or an
-- expected phenomenon in the case of TAFs.
data WeatherPhenomenon
    = Phenomenon
    { -- | The intensity of the phenomenon.
      intensity :: WPIntensity
    , -- | The description of the weather phenomenon.
      desc      :: Maybe WPDesc
    , -- | The precipitation type of the weather phenomenon.
      prec      :: Maybe WPPrecipitation
    , -- | The effects of the phenomenon on the visibility
      obfus     :: Maybe WPObfuscation
    , -- | Other details about the phenomenon.
      other     :: Maybe WPOther }
    deriving (Eq, Show)

-- | The intensity of an observed or expected weather phenomenon.
data WPIntensity
    = -- | Light
      Light
    | -- | Moderate
      Moderate
    | -- | Heavy
      Heavy
    | -- | Only applicable to METARs. The weather phenomenon was
      -- observed in the vicinity of the observed area, not within
      -- the observed area itself.
      Vicinity
    | -- | Only applicable to METARs. The weather phenomenon was
      -- recently observed in the past, but was not observed at
      -- the time the report was issued.
      Recent
    deriving (Enum, Eq, Show)

-- | The description of a weather phenomenon.
data WPDesc
    = -- | Shallow.
      Shallow
    | -- | Patches.
      Patches
    | -- | Partial.
      WXPartial
    | -- | Low, drifting.
      LowDrifting
    | -- | Blowing.
      Blowing
    | -- | Shower.
      Shower
    | -- | Thunderstorm.
      Thunderstorm
    | -- | Freezing.
      Freezing
    deriving (Enum, Eq, Ord, Show)

-- | The type of the precipitation
data WPPrecipitation
    = -- | Drizzle.
      Drizzle
    | -- | Rain.
      Rain
    | -- | Snow.
      Snow
    | -- | Snow grains.
      ShowGrains
    | -- | Ice crystals.
      IceCrystals
    | -- | Ice pellets.
      IcePellets
    | -- | Hail.
      Hail
    | -- | Snow pellets.
      SnowPellets
    | -- | Unknown type of precipitation.
      UnknownPrecipitation
    deriving (Enum, Eq, Ord, Show)

-- | Effects on the visibility by a weather phenomenon
data WPObfuscation
    = -- | Mist. Visibility impaired but still greater than 1000m
      Mist
    | -- | Fog. Visibility less than 1000m.
      Fog
    | -- | Smoke.
      Smoke
    | -- | Volcanic ash.
      VolcanicAsh
    | -- | Dust.
      Dust
    | -- | Sand.
      Sand
    | -- | Haze.
      Haze
    deriving (Enum, Eq, Ord, Show)

-- | Other important information about a weather phenomenon.
data WPOther
    = -- | Dust or sand whirls.
      DustOrSandwhirls
    | -- | Squalls.
      Squalls
    | -- | Tornado.
      Tornado
    | -- | Sand storm.
      Sandstorm
    | -- | Dust storm.
      Duststorm
    deriving (Enum, Eq, Ord, Show)

-- | The Distance.
data Distance
    = -- | The distance in metres.
      Metres Int
    | -- | The distance in km.
      KM Int
    | -- | The distance in statute miles.
      SM Int
    | -- | The distance in nautical miles.
      NM Int
    deriving (Eq, Show)

-- | Vertical visibility.
data Visibility
    = -- | Ten kilometres or more.
      TenOrMore
    | -- | Fifty metres or less.
      FiftyMetresOrLess
    | -- | Two kilometres or more.
      TwoOrMore
    | -- | A specific visibility.
      Visibility Distance (Maybe Direction)
    deriving (Eq, Show)

-- | Directions.
data Direction
    = -- | North.
      North
    | -- | South.
      South
    | -- | East.
      East
    | -- | West.
      West
    | -- | Northwest.
      NorthWest
    | -- | Northeast.
      NorthEast
    | -- | Southwest.
      SouthWest
    | -- | Southeast.
      SouthEast
    | -- | Left runway for runways of the same QFU
      -- (part of the runway designator)
      RWYLeft
    | -- | Right runway for runways of the same QFU
      -- (part of the runway designator)
      RWYRight
    | -- | Centre runway for runways of the same QFU
      -- (part of the runway designator)
      RWYCenter
    deriving (Eq, Show)

-- | Runway specification.
data Runway
    = -- | All runways.
      AllRunways
    | -- | A specific runway.
      Runway
    { -- | The runway's magnetic orientation, divided by ten and rounded.
      runwayQFU                 :: Int
    , -- | For multiple runways with the same QFU, a left, right or centre
      -- selector is added.
      runwayDirection           :: Maybe Direction }
    deriving (Eq, Show)

-- | The runway contamination type.
data RwyCoverType
    = -- | The runway is not contaminated.
      RCTDry
    | -- | The runway is moist.
      RCTMoist
    | -- | The runway is wet.
      RCTWet
    | -- | The runway is convered with rime.
      RCTRime
    | -- | The runway is covered with dry snow.
      RCTDrySnow
    | -- | The runway is covered with wet snow.
      RCTWetSnow
    | -- | The runway is covered with slush.
      RCTSlush
    | -- | The runway is covered with ice.
      RCTIce
    | -- | The runway is covered with frozen ruts or ridges.
      RCTFZRut
    | -- | The runway contamination type is unknown.
      RCTUnknown
    deriving (Eq, Show, Enum)

-- | Runway conditions.
data RunwayCondition
    = -- | Specific runway conditions exist.
      RunwayCondition
    { -- | The runway for which specific conditions
      -- have been observed.
      rwcondRunway :: Runway
    , -- | Whether and how the runway is contamindated.
      rwcondCover  :: RwyCoverType
    , -- | The extent of the contamination in percent.
      rwcondSpread :: Maybe Int
    , -- | The height of the contamination in millimetres.
      rwcondCoverHeight :: Maybe Int
    , -- | The friction coefficient or braking action value.
      rwcondBrkCoeff :: RunwayBraking }
    | -- | The runway is closed.
      RwyClosed
    { -- | The runway that is closed.
      rwclosedRunway :: Runway }
    | -- | The whole aerodrome is closed.
      ADClosed
    deriving (Eq, Show)

-- | The measured brake efficiency of a specific runway.
data RunwayBraking
    = -- | The friction coefficient.
      BrakingFriction Int
    | -- | The braking coefficient.
      BrakingEffect Int
    deriving (Eq, Show)

-- | An obersvation date.
data Date
    = Date Int Int Int
    deriving (Eq, Show)

-- | An aeronautical weather station designator.
data Station
    = -- | The station as identified by its aerodrome's
      -- ICAO code.
      ICAO Text
    deriving (Eq, Show)

-- | A vertical position specification.
data Vertical
    = -- | A vertical position with reference to the ground
      -- in feet.
      Height Int
    | -- | A vertical position with reference to the mean
      -- sea level/QNH in feet.
      Altitude Int
    | -- | A pressure altitude with reference to the
      -- standard QNH of 1013 hectopascals in hundrets of feet.
      FlightLevel Int
    deriving (Eq, Show)

-- | Wind information.
data Wind
    = Wind
    { -- | The direction the wind is blowing from.
      direction :: WindDirection
    , -- | The wind speed.
      velocity  :: Unit
    , -- | The strength of the observed gusts, if any.
      gusts     :: Maybe Int
    } deriving (Eq, Show)

-- | The direction the wind is blowing from.
data WindDirection
    = -- | The wind is blowing in equal or almost equal
      -- strength from a wide variety of directions.
      Variable
    | -- | The wind is blowing from the specified direction.
      -- Directions can be given with reference to true or
      -- magnetic north, depending on the type of weather
      -- observation/forecast message.
      Degrees Int
    | -- | The wind is blowing from a specific direction
      -- range.
      Varying
    { -- | The mean direction the wind is blowing from.
      mean :: Int
    , -- | The minimum direction the wind is blowing from.
      from :: Int
    , -- | The maximum direction the wind is blowing from.
      to   :: Int
    } deriving (Eq, Show)
     

-- | A speed unit.
data Unit
    = -- | Nautical miles per hour
      Knots Int
    | -- | Statute miles per hour
      Miles Int
    | -- | Kilometres per hour
      KMH Int
    deriving (Eq, Show)

-- | A cloud specification.
data Cloud
    = -- | No specific clouds could be observed, because
      -- the (given) ground visibility was too low or because
      -- the ground is covered in clouds.
      VVis (Maybe Int)
    | -- | Clouds were observed.
      Cloud Cover Vertical CloudType
    deriving (Eq, Show)

-- | The type of cloud.
data CloudType
    = -- | A cumulonimbus cloud.
      Cumulonimbus
    | -- | A developing cb cloud.
      ToweringCumulus
    | -- | A stratus cloud.
      Stratus
    | -- | A cumulus cloud.
      Cumulus
    | -- | A stratocumulus cloud.
      Stratocumulus
    | -- | An altostratus cloud.
      Altostratus
    | -- | An altocumulus cloud.
      Altocumulus
    | -- | A cirrostratus cloud.
      Cirrostratus
    | -- | A cirrus cloud.
      Cirrus
    | -- | An unclassified cloud.
      Unclassified
    deriving (Enum, Eq, Show)

-- | The area that is covered.
data Cover
    = -- | 1-2 eights are covered.
      FEW
    | -- | 3-4 eights are covered.
      SCT
    | -- | 5-7 eights are covered.
      BKN
    | -- | More than 7 eights are covered.
      OVC
    deriving (Enum, Eq, Ord, Show)

stationParser :: Parser Station
stationParser = ICAO <$> Data.Attoparsec.Text.take 4

dateParser :: Parser Date
dateParser = Date <$> twin <*> twin <*> (twin <* "Z")
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
        readunit = choice [ "KT" `means` Knots
                          , "MPH" `means` Miles
                          , "KM" `means` KMH]
        readgusts = (\_ b c -> Just . read $ [b, c]) <$> char 'G' <*> digit <*> digit

pressureParser :: Parser Pressure
pressureParser = choice [qnh, mmhg]
    where
      qnh = (\_ a b c d -> QNH $ read [a, b, c, d]) <$> char 'Q' <*> digit <*> digit <*> digit <*> digit
      mmhg = (\_ a b c d -> Altimeter $ read [a, b, c, d]) <$> char 'A' <*> digit <*> digit <*> digit <*> digit

wxParser :: Parser WeatherPhenomenon
wxParser = do
    skipSpace
    intsy <- intensityParser
    dsc <- perhaps descParser
    prc <- perhaps precipitationParser
    obfs <- perhaps obfuscationParser
    othr <- perhaps otherParser
    when ( (== 0) . Prelude.length . Prelude.filter not $
        [ isNothing dsc, isNothing prc
        , isNothing obfs, isNothing othr ] ) $ fail ""
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
descParser = choice
    [ "MI" `means` Shallow
    , "BC" `means` Patches
    , "PR" `means` WXPartial
    , "DR" `means` LowDrifting
    , "BL" `means` Blowing
    , "SH" `means` Shower
    , "TS" `means` Thunderstorm
    , "FZ" `means` Freezing ]

precipitationParser :: Parser WPPrecipitation
precipitationParser = choice
    [ "DZ" `means` Drizzle
    , "RA" `means` Rain
    , "SN" `means` Snow
    , "SG" `means` ShowGrains
    , "IC" `means` IceCrystals
    , "PL" `means` IcePellets
    , "GR" `means` Hail
    , "GS" `means` SnowPellets
    , "UP" `means` UnknownPrecipitation ]

obfuscationParser :: Parser WPObfuscation
obfuscationParser = choice
    [ "BR" `means` Mist
    , "FG" `means` Fog
    , "FU" `means` Smoke
    , "VA" `means` VolcanicAsh
    , "DU" `means` Dust
    , "SA" `means` Sand
    , "HZ" `means` Haze ]

otherParser :: Parser WPOther
otherParser = choice
    [ "PO" `means` DustOrSandwhirls
    , "SQ" `means` Squalls
    , "FC" `means` Tornado
    , "SS" `means` Sandstorm
    , "DS" `means` Duststorm ]

intensityParser :: Parser WPIntensity
intensityParser = option Moderate $ choice
    [ char '-' >> return Light
    , char '+' >> return Heavy
    , "VC" `means` Vicinity
    , "RE" `means` Recent ]

visibilityParser :: Parser Visibility
visibilityParser = skipSpace >> choice [ tenormore, arb, arb1, metres ]
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
directionParser = Nothing `option` (Just <$> choice
    [ "NE" `means` NorthEast, "NW" `means` NorthWest
    , "SE" `means` SouthEast, "SW" `means` SouthWest
    , "N" `means` North, "S" `means` South
    , "E" `means` East, "W" `means` West ])

distanceUnitParser :: Parser (Int -> Distance)
distanceUnitParser = choice
    [ "KM" `means` KM
    , "SM" `means` SM
    , "NM" `means` NM ]

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
    choice
        [ "///" `means` VVis Nothing
        , (\a b c -> VVis . Just . read $ [a,b,c]) <$> digit <*> digit <*> digit ]

cloudIntensityParser :: Parser Cover
cloudIntensityParser = choice
    [ "FEW" `means` FEW
    , "SCT" `means` SCT
    , "BKN" `means` BKN
    , "OVC" `means` OVC ]

cloudTypeParser :: Parser CloudType
cloudTypeParser = option Unclassified $ choice
    [ "CB" `means` Cumulonimbus
    , "TCU" `means` ToweringCumulus
    , "ST" `means` Stratus
    , "CU" `means` Cumulus
    , "SC" `means` Stratocumulus
    , "AS" `means` Altostratus
    , "AC" `means` Altocumulus
    , "CS" `means` Cirrostratus
    , "CI" `means` Cirrus ]

perhapsMinus :: Parser String
perhapsMinus = "" `option` (char 'M' >> return "-")

tdParser :: Parser (Int, Int)
tdParser = do
    tmpr <- (\pm a b -> read (pm ++ [a, b]) :: Int) <$> perhapsMinus <*> digit <*> digit
    _ <- char '/'
    dewpoint <- (\pm a b -> read (pm ++ [a, b]) :: Int) <$> perhapsMinus <*> digit <*> digit
    return (tmpr, dewpoint)

flagsParser :: Parser [Flag]
flagsParser = many' $ choice
    [ "COR" `means'` COR
    , "AMD" `means'` AMD
    , "AUTO" `means'` AUTO ]

runwayvisParser :: Parser (Runway, [Visibility], Maybe VisTrend)
runwayvisParser = do
    runway <- runwayDesignationParser
    _ <- char '/'
    vis <- parseRwyVis
    vistrend <- Nothing `option` (Just <$> choice
        [ "D" `means` VisTrendDownward
        , "N" `means` VisTrendNoDistinctTendency
        , "U" `means` VisTrendUpward ] )
    return (runway, vis, vistrend)
    where
        parseRwyVis :: Parser [Visibility]
        parseRwyVis = do
            worstvis <- Nothing `option` (Just <$> choice visspec <* "V")
            vis <- Just <$> choice visspec
            return $ catMaybes [worstvis, vis]
                        
        visspec =
            [ "M0050" `means` FiftyMetresOrLess
            , "P2000" `means` TwoOrMore
            , fourDigits >>= \a -> return $ Visibility (Metres a) Nothing
            , trieDigits >>= \a -> return $ Visibility (Metres a) Nothing ]
                        
runwayconditionParser :: Parser RunwayCondition
runwayconditionParser = do
    runway <- runwayDesignationParser
    _ <- char '/'
    choice
        [ "SNOCLO" `means` ADClosed
        , rwycond runway ]

    where
        rwycond runway = do
            cover <- RCTUnknown `option` ((toEnum . read . (:[])) <$> digit)
            spread <- choice
                [ char '/' >> return Nothing
                , (Just . read . (:[])) <$> digit ]
            spreadheight <- choice
                [ string "//" >> return Nothing
                , Just <$> tuhDigits ]
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
            dir <- Nothing `option` (Just <$> choice
                [ "L" `means` RWYLeft
                , "R" `means` RWYRight
                , "C" `means` RWYCenter ])
            return $ Runway magheading dir

trendParser :: Parser Trend
trendParser = choice
    [ "NOSIG" `means` NOSIG
    , changeParser ]
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
        transitionTypeParser = choice
                [ "TEMPO" `callsfor` (TEMPO <$> transitionParser)
                , "BECMG" `callsfor` (BECMG <$> transitionParser)
                , "PROB" `callsfor`  (PROB  <$> twoDigits <*> (head <$> changesParser)) ]
        transitionParser :: Parser [Transition]
        transitionParser = sepBy1' oneTransition (char ' ')
        oneTransition = do
            skipSpace
            choice . map try $
              [ TransClouds    <$> cloudParser
              , TransWind      <$> windParser
              , TransVis       <$> many1' visibilityParser
              , TransWX        <$> count 1 wxParser
              , TransRunwayVis <$> sepBy' runwayvisParser (char ' ') ]

twoDigits :: Parser Int
twoDigits = (\a b -> read [a,b]) <$> digit <*> digit

metarParser :: Parser Weather
metarParser = do
    _ <- string "METAR"
    reportflags <- flagsParser
    identifier <- skipSpace >> stationParser
    reportdate <- skipSpace >> dateParser
    reportflags2 <- flagsParser
    reportwind <- Nothing `option` (skipSpace >> Just <$> windParser)
    skipSpace
    reportvis <- [TenOrMore] `option` many1 visibilityParser
    skipSpace
    reportrunwaycond <- sepBy' runwayconditionParser (char ' ')
    reportrunwayvis <- sepBy' runwayvisParser (char ' ')
    reportwx <- many' wxParser
    reportclouds <- [] `option` (skipSpace >> cloudParser)
    (reporttemp, reportdewpoint) <- (Nothing, Nothing) `option` (skipSpace >> tdParser >>= \(w,d) -> return (Just w, Just d))
    reportpressure <- Nothing `option` (skipSpace >> Just <$> pressureParser)
    void $ many' $ skipSpace >> pressureParser -- Sometimes, multiple pressure values are offered
    skipSpace
    reporttrend <- NOTAVAIL `option` trendParser
    reportrmk <- maybeRMK
    skipSpace
    _ <- choice $ map char "=$"
    return $ METAR reportdate identifier (reportflags ++ reportflags2)
        reportwind reportvis reportrunwayvis reportrunwaycond reportwx
        reportclouds reportpressure reporttemp reportdewpoint
        reporttrend reportrmk

maybeRMK :: Parser (Maybe Text)
maybeRMK = Nothing `option` do
    void $ choice [ string "RMK ", string " RMK " ]
    Just . pack <$> many1 (satisfy (notInClass "$="))

-- | An attoparsec parser that can parse METAR messages.
weatherParser :: Parser Weather
weatherParser = metarParser

maybeOneSpace :: Parser ()
maybeOneSpace = perhaps_ space

-- | Parse the given METAR text.
parseWeather :: Text -> Either String Weather
parseWeather = parseOnly weatherParser
