{-# OPTIONS_GHC -Wall #-}
module State where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Coordinate
import Matrix as MX
import Model


-- | The state S of an executing Nanobot Matter Manipulation System
data SystemState
  = SystemState
  { stEnergy :: !Integer
  , stHarmonics :: !Bool
  , stResolution :: !Int -- TODO: Matrix自体に持たせる
  , stMatrix :: Matrix
  , stBots :: !(IntMap Bot) -- BotId to Bot mapping
  , stTrace :: Trace
  } deriving (Eq, Ord, Show)

stateIsWellformed :: SystemState -> Bool
stateIsWellformed _ = True -- todo

initialState :: Model -> Trace -> SystemState
initialState (Model res _mat) trace =
  SystemState
  { stEnergy = 0
  , stHarmonics = False
  , stResolution = res
  , stMatrix = makeMatrix []
  , stBots = IntMap.singleton 1 $
      Bot
      { botId = 1
      , botPos = Coord (0,0,0)
      , botSeeds = IntSet.fromList [2..40] -- dup seeds in FullContest
      }
  , stTrace = trace
  }


type BotId = Int

-- | The state of an active nanobot bot
data Bot
  = Bot
  { botId  :: !BotId  -- ^ bid in doc
  , botPos :: !Coord
  , botSeeds :: !IntSet -- set of BotId
  } deriving (Eq, Ord, Show)

data Command
  = Halt
  | Wait
  | Flip
  | SMove !LLD
  | LMove !SLD !SLD
  | Fission !ND !Int
  | Fill !ND
  | Void !ND
  | GFill !ND !FD
  | GVoid !ND !FD
  | FusionP !ND
  | FusionS !ND
  deriving (Eq, Ord, Show)

type Trace = [Command]
