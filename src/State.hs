module State where

import Control.Monad
import qualified Control.Monad.State as SM
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.Set (Set)

import Coordinate
import Matrix

-- | The state S of an executing Nanobot Matter Manipulation System
data State
  = State
  { stEnergy :: !Integer
  , stHarmonics :: !Bool
  , stResolution :: !Int -- TODO: Matrix自体に持たせる
  , stMatrix :: Matrix
  , stBots :: IntMap Bot -- BotId to Bot mapping
  , stTrace :: Trace
  } deriving (Eq, Ord, Show)

stateIsWellformed :: State -> Bool
stateIsWellformed _ = True -- todo


type BotId = Int

-- | The state of an active nanobot bot
data Bot
  = Bot
  { botId  :: BotId  -- ^ bid in doc
  , botPos :: Coord
  , botSeeds :: IntSet -- set of BotId
  } deriving (Eq, Ord, Show)

data Command
  = Halt
  | Wait
  | Flip
  | SMove LLD
  | LMove SLD SLD
  | Fission ND Int
  | Fill ND
  | FusionP ND
  | FusionS ND
  deriving (Eq, Ord, Show)

type Trace = [Command]


execOneStep :: SM.State State ()
execOneStep = do
  s <- SM.get
  unless (stateIsWellformed s) $ error "state is not wellformed"
  let n = IntMap.size (stBots s)
  case splitAt n (stTrace s) of
    (xs, traces') -> do
      unless (length xs == n) $ error "trace is short"
      execBots $ zip (map fst (IntMap.toAscList (stBots s))) xs
      s <- SM.get
      if stHarmonics s then
        addCost $ 30 * stResolution s ^ (3 :: Int)
      else
        addCost $ 3 * stResolution s ^ (3 :: Int)
      addCost $ 20 * n -- nanobotの個数は実行前の個数で良いのだろうか?
      SM.modify (\s -> s{ stTrace = traces' })

addCost :: Int -> SM.State State ()
addCost cost = SM.modify (\s -> s{ stEnergy = stEnergy s + fromIntegral cost })

execBots :: [(BotId, Command)] -> SM.State State ()
execBots xs = do
  s <- SM.get

  let fusionPs = Map.fromList [(c, (bid, c `add` nd)) | (bid,FusionP nd) <- xs, let c = botPos (stBots s IntMap.! bid)]
      fusionSs = Map.fromList [(c, (bid, c `add` nd)) | (bid,FusionS nd) <- xs, let c = botPos (stBots s IntMap.! bid)]
  unless (Map.size fusionPs == Map.size fusionSs) $ error "failed to create a pair for fusion"
  let fusionPairs = do
        (c1,(bid1,c2)) <- Map.toList fusionPs
        case Map.lookup c2 fusionSs of
          Just (bid2,c1') | c1==c1' -> return (bid1,bid2)
          _ -> error "failed to create a pair for fusion"

  let mat = stMatrix s
  forM_ xs $ \x@(bid,cmd) -> do
    case cmd of
      FusionP _ -> return ()
      FusionS _ -> return ()
      _ -> execSingleNanobotCommand mat bid cmd
  forM_ fusionPairs $ uncurry execFusion


-- 事前条件のチェックは他のボットのコマンドの実行前のmatrixに対して行う必要があるので、
-- そのMatrixを受け取っている。
execSingleNanobotCommand :: Matrix -> BotId -> Command -> SM.State State ()
execSingleNanobotCommand _mat _bid Halt = do
  s <- SM.get
  case IntMap.elems (stBots s) of
    [bot] -> do
      unless (botPos bot == Coord (0,0,0)) $ error "Halt pre-condition is violated"
      SM.put $ s{ stBots = IntMap.empty }
    _ -> error "Halt pre-condition is violated"
execSingleNanobotCommand _mat _bid Wait = return ()
execSingleNanobotCommand _mat _bid Flip = do
  s <- SM.get
  SM.put $ s{ stHarmonics = not (stHarmonics s) }
execSingleNanobotCommand mat bid (SMove lld) = do
  s <- SM.get
  case IntMap.lookup bid (stBots s) of
    Just bot -> do
      let c  = botPos bot
          c' = add c lld
      unless (checkVoidRegion mat (region c c')) $ error "SMove pre-condition is violated"
      SM.put $
        s{ stBots   = IntMap.insert bid bot{ botPos = c' } (stBots s)
         , stEnergy = stEnergy s + fromIntegral (2 * mlen lld)
         }
execSingleNanobotCommand mat bid (LMove sld1 sld2) = do
  s <- SM.get
  case IntMap.lookup bid (stBots s) of
    Just bot -> do
      let c   = botPos bot
          c'  = add c sld1
          c'' = add c' sld2
      unless (checkVoidRegion mat (region c  c' )) $ error "LMove pre-condition is violated"
      unless (checkVoidRegion mat (region c' c'')) $ error "LMove pre-condition is violated"
      SM.put $
        s{ stBots   = IntMap.insert bid bot{ botPos = c'' } (stBots s)
         , stEnergy = stEnergy s + fromIntegral (2 * mlen sld1 + 2 + mlen sld2)
         }
execSingleNanobotCommand mat bid (Fission nd m) = do
  s <- SM.get
  case IntMap.lookup bid (stBots s) of
    Just bot -> do
      when (IntSet.null (botSeeds bot)) $ error "Fission pre-condition is violated"
      let c = botPos bot
          c' = add c nd
      unless (isVoid mat c') $ error "Fission pre-condition is violated"
      case splitAt (m+1) (IntSet.toAscList (botSeeds bot)) of
        (bid':seeds1, seeds2) -> do
          SM.put $
            s{ stBots =
                 IntMap.insert bid  bot{ botSeeds = IntSet.fromAscList seeds2 } $
                 IntMap.insert bid' Bot{ botId = bid', botPos = c', botSeeds = IntSet.fromAscList seeds1 } $
                 stBots s
             , stEnergy = stEnergy s + 24
             }
execSingleNanobotCommand _mat bid (Fill nd) = do
  s <- SM.get
  case IntMap.lookup bid (stBots s) of
    Just bot -> do
      let c = botPos bot
          c' = add c nd
          mat = stMatrix s
      case voxel mat c' of
        Void -> do
          SM.put $ s{ stMatrix = fill c' mat, stEnergy = stEnergy s + 12 }
        Full -> do
          SM.put $ s{ stEnergy = stEnergy s + 6 }


execFusion :: BotId -> BotId -> SM.State State ()
execFusion bidP bidS = do
  s <- SM.get
  case IntMap.lookup bidP (stBots s) of
    Just botP ->
      case IntMap.lookup bidS (stBots s) of
        Just botS -> do
          SM.put $
            s{ stBots =
                 IntMap.insert bidP botP{ botSeeds = IntSet.insert bidS $ botSeeds botS `IntSet.union` botSeeds botP } $
                 IntMap.delete bidS $
                 stBots s
             , stEnergy = stEnergy s - 24
             }


checkVoidRegion :: Matrix -> Region -> Bool
checkVoidRegion mat r = all (isVoid mat) (membersOfRegion r)
