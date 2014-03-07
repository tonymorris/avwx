module Aeroplane where

newtype Mass = Mass Float

data Aeroplane = Aeroplane {
  aptype :: AeroplaneType,
  operation :: [AeroplaneOperation],
  equipment :: [AeroplaneEquipment],
  apwbdata :: WBData,
  apmtow :: Mass
} deriving (Eq, Show)

data AeroplaneEquipment = EquipmentTBD deriving (eq, Show)

data AeroplaneType = SEP | MEP | Turboprop | Jet deriving (Eq, Show)

data AeroplaneOperation = VFR | VFRN | IFR deriving (Eq, Show)

data Arm = Arm {
  arm :: Double,
  armdesignation :: String
} deriving (Eq, Show)

data WBData = WBData {
  wbempty :: Mass,
  wbbasic :: Mass,
  wbusable :: Mass,
  wbarms :: [Arm]
  } deriving (Eq, Show)
             
             
main = putStrLn "OHAI"

