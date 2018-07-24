{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module State where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
-- import Data.Set (Set)
import qualified Data.Set as Set

import Coordinate
import Matrix as MX
import Model


data Harmonics = High | Low
  deriving (Eq, Ord, Show, Bounded, Enum)

flipHarmonics :: Harmonics -> Harmonics
flipHarmonics High = Low
flipHarmonics Low = High


-- | The state S of an executing Nanobot Matter Manipulation System
data SystemState
  = SystemState
  { stEnergy :: !Integer
  , stHarmonics :: !Harmonics
  , stResolution :: !Int -- TODO: Matrix自体に持たせる
  , stMatrix :: Matrix
  , stTgtMatrix :: Matrix   -- Target
  , stSrcMatrix :: Matrix   -- Source
  , stBots :: !(IntMap Bot) -- BotId to Bot mapping
  , stTrace :: Trace
  , stTime :: !Int
  , stCommands :: !Int
  , stGroundedTable :: GroundedTable
  } deriving (Eq, Ord, Show)


stateIsWellformed :: SystemState -> Bool
-- stateIsWellformed s | isGrounded (stMatrix s) /= stateIsGrounded s = undefined
stateIsWellformed s = and
  [ -- If the harmonics is Low, then all Full voxels of the matrix are grounded.
    stHarmonics s == High || stateIsGrounded s
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


stateIsGrounded :: SystemState -> Bool
stateIsGrounded = isAllGrounded . stGroundedTable


-- for lightning division
initialStateForAssemblyL :: Model -> Trace -> SystemState
initialStateForAssemblyL tgtModel trace =
  SystemState
  { stEnergy = 0
  , stHarmonics = Low
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
  , stTime = 0
  , stCommands = 0
  , stGroundedTable = emptyGroundedTable
  }

initialStateForReassembly :: Model -> Model -> Trace -> SystemState
initialStateForReassembly srcModel tgtModel trace =
  SystemState
  { stEnergy = 0
  , stHarmonics = Low
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
  , stTime = 0
  , stCommands = 0
  , stGroundedTable = foldl' (flip fillGroundedTable) emptyGroundedTable (matrixCoords (mdMatrix srcModel))
  }

initialStateForAssembly :: Model -> Trace -> SystemState
initialStateForAssembly tgtModel trace =
  initialStateForReassembly (emptyR tgtModel) tgtModel trace

initialStateForDisassembly :: Model -> Trace -> SystemState
initialStateForDisassembly srcModel trace =
  initialStateForReassembly srcModel (emptyR srcModel) trace


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
  deriving (Eq, Ord, Show, Generic)

instance NFData Command

type Trace = [Command]

noActiveNanobots :: SystemState -> Bool
noActiveNanobots = IntMap.null . stBots

noCommands :: SystemState -> Bool
noCommands = null . stTrace



data GroundedTable
  = GroundedTable
  { gtRepr :: !(Map Coord Coord) -- 各filledなvoxedから代表元への写像
  , gtClusters :: !(Map Coord Matrix) -- 代表元からそのクラスタに属するボクセルの集合への写像
  }
  deriving (Eq, Ord, Show)

emptyGroundedTable :: GroundedTable
emptyGroundedTable =
  GroundedTable
  { gtRepr = Map.empty
  , gtClusters = Map.empty
  }

makeGroundedTable :: [Coord] -> GroundedTable
makeGroundedTable = foldl' (flip fillGroundedTable) emptyGroundedTable

isAllGrounded :: GroundedTable -> Bool
isAllGrounded = all MX.isSomeGrounded . gtClusters

getRepr :: GroundedTable -> Coord -> Coord
getRepr gt v = gtRepr gt Map.! v

lookupCluster :: Coord -> GroundedTable -> Matrix
lookupCluster v gt = gtClusters gt Map.! (getRepr gt v)

mergeClusters :: Coord -> Coord -> GroundedTable -> GroundedTable
mergeClusters v1 v2 gt
  | v1' == v2' = gt
  | matrixSize m1 >= matrixSize m2 =
      GroundedTable
      { gtRepr = Map.fromList [(c,v1') | c <- matrixCoords m2] `Map.union` gtRepr gt
      , gtClusters = Map.insert v1' (matrixUnion m1 m2) $ Map.delete v2' $ gtClusters gt
      }
  | otherwise =
      GroundedTable
      { gtRepr = Map.fromList [(c,v2') | c <- matrixCoords m1] `Map.union` gtRepr gt
      , gtClusters = Map.insert v2' (matrixUnion m1 m2) $ Map.delete v1' $ gtClusters gt
      }
  where
    v1' = getRepr gt v1
    v2' = getRepr gt v2
    m1 = gtClusters gt Map.! v1'
    m2 = gtClusters gt Map.! v2'

fillGroundedTable :: Coord -> GroundedTable -> GroundedTable
fillGroundedTable v@(Coord (x,y,z)) gt
  | v `Map.member` gtRepr gt = gt
  | otherwise = foldl' f gt0 neighbors
  where
    gt0 =
      GroundedTable
      { gtRepr = Map.insert v v (gtRepr gt)
      , gtClusters = Map.insert v (makeMatrix [v]) (gtClusters gt)
      }
    neighbors = map Coord [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]
    f gt1 v1
      | v1 `Map.member` gtRepr gt1 = mergeClusters v v1 gt1
      | otherwise = gt1


voidGroundedTableSimple :: Coord -> GroundedTable -> Maybe GroundedTable
voidGroundedTableSimple v@(Coord (x,y,z)) gt =
  case Map.lookup v (gtRepr gt) of
    Nothing -> Just gt
    Just r ->
      let m = gtClusters gt Map.! r
          neighbors  = [v1 | v1 <- map Coord [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)], v1 `Map.member` gtRepr gt]
          neighbors2 = [v1 | v1 <- map Coord [(x1,y1,z1) | x1 <- [x-1..x+1], y1 <- [y-1..y+1], z1 <- [z-1..z+1]], v /= v1, v1 `Map.member` gtRepr gt]
      in if matrixSize m == 1 then -- この場合 v = r
           Just $ GroundedTable
           { gtRepr = Map.delete v (gtRepr gt)
           , gtClusters = Map.delete r (gtClusters gt)
           }
         else if length neighbors < 2 || Map.size (gtClusters (makeGroundedTable neighbors2)) <= 1 then -- 隣接するボクセル同士は近くで繋がっている
           if v == r then
             -- この場合には代表元を他のものに変更する必要がある
             let r2 = head [v' | v' <- matrixCoords m, v' /= v] -- 新しい代表元
             in Just $ GroundedTable
                { gtRepr = Map.fromList [(v',r2) | v' <- matrixCoords m, v' /= v] `Map.union` Map.delete v (gtRepr gt) -- left biased
                , gtClusters = Map.insert r2 (MX.void v m) $ Map.delete v $ gtClusters gt
                }
           else
             Just $ GroundedTable
             { gtRepr = Map.delete v $ gtRepr gt
             , gtClusters = Map.insert r (MX.void v m) $ gtClusters gt
             }
         else
           Nothing

voidGroundedTable :: [Coord] -> GroundedTable -> GroundedTable
voidGroundedTable = f []
  where
    f vs' [] gt = voidGroundedTable' vs' gt
    f vs' (v : vs) gt =
      case voidGroundedTableSimple v gt of
        Just gt' -> f vs' vs gt'
        Nothing -> f (v : vs') vs gt

voidGroundedTable' :: [Coord] -> GroundedTable -> GroundedTable
voidGroundedTable' [] gt = gt
voidGroundedTable' vs gt =
  GroundedTable
  { gtRepr = Map.unions $
      (gtRepr gt Map.\\ Map.fromSet (const ()) cs) : [gtRepr gt1 | gt1 <- gts]
  , gtClusters = Map.unions $
      (gtClusters gt Map.\\ Map.fromSet (const ()) rs) : [gtClusters gt1 | gt1 <- gts]
  }
  where
    vs' = Set.fromList vs
    rs = Set.fromList $ catMaybes [Map.lookup v (gtRepr gt) | v <- vs]
    cs = Set.unions [Set.fromList $ matrixCoords $ gtClusters gt Map.! r | r <- Set.toList rs]
    gts = do
      r <- Set.toList rs
      return $ makeGroundedTable $ Set.toList $
        Set.fromList (matrixCoords (gtClusters gt Map.! r)) Set.\\ vs'
