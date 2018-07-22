{-# OPTIONS_GHC -Wall #-}
module State where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

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
  , stTgtMatrix :: Matrix   -- Target
  , stSrcMatrix :: Matrix   -- Source
  , stBots :: !(IntMap Bot) -- BotId to Bot mapping
  , stTrace :: Trace
  } deriving (Eq, Ord, Show)


stateIsWellformed :: SystemState -> Bool
stateIsWellformed s = and
  [ -- If the harmonics is Low, then all Full voxels of the matrix are grounded.
    stHarmonics s || isGrounded (stMatrix s)
  , -- Each active nanobot has a different identifier.
    and [botId bot == bid | (bid,bot) <- IntMap.toList (stBots s)]
  , -- The position of each active nanobot is distinct and is Void in the matrix.
    all (<=1) $ Map.fromListWith (+) [(botPos bot, 1::Int) | bot <- IntMap.elems (stBots s)]
  , and [isEmpty (stMatrix s) (botPos bot) | bot <- IntMap.elems (stBots s)]
  , -- The seeds of each active nanobot are disjoint.
    all (<=1) $ IntMap.unionsWith (+) [IntMap.fromAscList [(seed, 1::Int) | seed <- IntSet.toAscList (botSeeds bot)] | bot <- IntMap.elems (stBots s)]
  , -- The seeds of each active nanobot does not include the identifier of any active nanobot.
    IntSet.null $
      IntSet.unions [botSeeds bot | bot <- IntMap.elems (stBots s)]
      `IntSet.intersection`
      IntMap.keysSet (stBots s)
  ]


-- for lightning division
initialStateForAssemblyL :: Model -> Trace -> SystemState
initialStateForAssemblyL tgtModel trace =
  SystemState
  { stEnergy = 0
  , stHarmonics = False
  , stResolution = mdResolution tgtModel
  , stMatrix = MX.makeMatrix []
  , stTgtMatrix = mdMatrix tgtModel
  , stSrcMatrix = MX.makeMatrix []
  , stBots = IntMap.singleton 1 $
      Bot
      { botId = 1
      , botPos = Coord (0,0,0)
      , botSeeds = IntSet.fromList [2..20]
      }
  , stTrace = trace
  }

initialStateForReassembly :: Model -> Model -> Trace -> SystemState
initialStateForReassembly srcModel tgtModel trace =
  SystemState
  { stEnergy = 0
  , stHarmonics = False
  , stResolution = mdResolution tgtModel
  , stMatrix = mdMatrix srcModel
  , stTgtMatrix = mdMatrix tgtModel
  , stSrcMatrix = mdMatrix srcModel
  , stBots = IntMap.singleton 1 $
      Bot
      { botId = 1
      , botPos = Coord (0,0,0)
      , botSeeds = IntSet.fromList [2..40] -- dup seeds in FullContest
      }
  , stTrace = trace
  }

initialStateForAssembly :: Model -> Trace -> SystemState
initialStateForAssembly tgtModel@(Model res _mat) trace =
  initialStateForReassembly (Model res (MX.makeMatrix [])) tgtModel trace

initialStateForDisassembly :: Model -> Trace -> SystemState
initialStateForDisassembly srcModel@(Model res _mat) trace =
  initialStateForReassembly srcModel (Model res (MX.makeMatrix [])) trace


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
