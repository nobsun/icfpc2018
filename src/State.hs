module State where

import Control.Monad
import qualified Control.Monad.State as SM
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.Ord
import Data.Set (Set)

import Coordinate
import Matrix

-- | The state S of an executing Nanobot Matter Manipulation System
data State
  = State
  { stEnergy :: !Integer
  , stHarmonics :: !Bool
  , stMatrix :: Matrix
  , stBots :: IntMap Bot -- BotId to Bot mapping
  , stTrace :: Trace
  } deriving (Eq, Ord, Show)

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

execSingleNanobotCommand :: BotId -> Command -> SM.State State ()
execSingleNanobotCommand _bid Halt = do
  s <- SM.get
  case IntMap.elems (stBots s) of
    [bot] -> do
      unless (botPos bot == Coord (0,0,0)) $ error "Halt pre-condition is violated"
      SM.put $ s{ stBots = IntMap.empty }
    _ -> error "Halt pre-condition is violated"
execSingleNanobotCommand _bid Wait = return ()
execSingleNanobotCommand _bid Flip = do
  s <- SM.get
  SM.put $ s{ stHarmonics = not (stHarmonics s) }
execSingleNanobotCommand bid (SMove lld) = do
  s <- SM.get
  case IntMap.lookup bid (stBots s) of
    Just bot -> do
      let c  = botPos bot
          c' = add c lld
          mat = stMatrix s
      unless (checkVoidRegion mat (region c c')) $ error "SMove pre-condition is violated"
      SM.put $
        s{ stBots   = IntMap.insert bid bot{ botPos = c' } (stBots s)
         , stEnergy = stEnergy s + fromIntegral (2 * mlen lld)
         }
execSingleNanobotCommand bid (LMove sld1 sld2) = do
  s <- SM.get
  case IntMap.lookup bid (stBots s) of
    Just bot -> do
      let c   = botPos bot
          c'  = add c sld1
          c'' = add c' sld2
          mat = stMatrix s
      unless (checkVoidRegion mat (region c  c' )) $ error "LMove pre-condition is violated"
      unless (checkVoidRegion mat (region c' c'')) $ error "LMove pre-condition is violated"
      SM.put $
        s{ stBots   = IntMap.insert bid bot{ botPos = c'' } (stBots s)
         , stEnergy = stEnergy s + fromIntegral (2 * mlen sld1 + 2 + mlen sld2)
         }
execSingleNanobotCommand bid (Fission nd m) = do
  s <- SM.get
  case IntMap.lookup bid (stBots s) of
    Just bot -> do
      when (IntSet.null (botSeeds bot)) $ error "Fission pre-condition is violated"
      let c = botPos bot
          c' = add c nd
          mat = stMatrix s
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
execSingleNanobotCommand bid (Fill nd) = do
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

execFusion :: (BotId,ND) -> (BotId,ND) -> SM.State State ()
execFusion (bidP,ndP) (bidS,ndS) = do
  s <- SM.get
  case IntMap.lookup bidP (stBots s) of
    Just botP ->
      case IntMap.lookup bidS (stBots s) of
        Just botS -> do
          unless (add (botPos botP) ndP == botPos botS && add (botPos botS) ndS == botPos botP) $
            error "Fusion pre-condition is violated"
          SM.put $
            s{ stBots =
                 IntMap.insert bidP botP{ botSeeds = IntSet.insert bidS $ botSeeds botS `IntSet.union` botSeeds botP } $
                 IntMap.delete bidS $
                 stBots s
             , stEnergy = stEnergy s - 24
             }

checkVoidRegion :: Matrix -> Region -> Bool
checkVoidRegion mat r = all (isVoid mat) (membersOfRegion r)
