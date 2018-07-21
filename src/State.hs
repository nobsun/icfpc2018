module State where

import Data.Set (Set)

import Coordinate
import Matrix

data State
  = State
  { stEnergy :: !Integer
  , stHarmonics :: !Bool
  , stMatrix :: Matrix
  , stBots :: [Bot]
  , stTrace :: Trace
  } deriving (Eq, Ord, Show)

data Bot
  = Bot
  { botId  :: Int
  , botPos :: Coord
  , botSeeds :: Set String
  } deriving (Eq, Ord, Show)

data Command
  = Halt
  | Wait
  | Flip
  | SMove LLD
  | LMove LLD LLD
  | Fission ND Int
  | Fill ND
  | FusionP ND
  | FusionS ND
  deriving (Eq, Ord, Show)

type Trace = [Command]
