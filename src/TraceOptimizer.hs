module TraceOptimizer where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List

import Matrix
import Model
import Sim
import State

optimize :: Model -> Model -> Trace -> Trace
optimize ini fini trace = concat [IntMap.elems cmds | cmds <- loop False (zip xs (tail (map fst xs) ++ [True])), not (all (== Wait) cmds)]
  where
    xs :: [(Bool, IntMap Command)]
    xs = f (initialStateForReassembly ini fini trace)
      where
        f :: SystemState -> [(Bool, IntMap Command)]
        f s
          | IntMap.null (stBots s) = []
          | otherwise = 
              ( isGrounded (stMatrix s)
              , IntMap.fromAscList $ [(bid, cmd) | ((bid,_),cmd) <- zip (IntMap.toAscList (stBots s)) (take n (stTrace s))]
              ) : f (execOneStep s)
          where
            n = IntMap.size (stBots s)

    loop :: Bool -> [((Bool, IntMap Command), Bool)] -> [IntMap Command]
    loop _ [] = []
    loop False (((_True, cmds), True) : ys)
      | Flip `elem` IntMap.elems cmds = replaceFlipWithWait cmds : loop False ys
      | otherwise = cmds : loop False ys
    loop False yys@(((_True, cmds), False) : ys)
      | Flip `elem` IntMap.elems cmds = cmds : loop True ys
      | Wait `elem` IntMap.elems cmds = replaceWait Flip cmds : loop True ys
      | otherwise = flipOnly cmds : cmds : loop True ys
    loop True yys@(((True, cmds), True) : ys)
      | Flip `elem` IntMap.elems cmds = cmds : loop False ys
      | Wait `elem` IntMap.elems cmds = replaceWait Flip cmds : loop False ys
      | otherwise = flipOnly cmds : loop False yys
    loop True (((False, cmds), True) : ys)
      | Flip `elem` IntMap.elems cmds = cmds : loop False ys
      | Wait `elem` IntMap.elems cmds = replaceWait Flip cmds : loop False ys
      | otherwise = cmds : loop True ys
    loop isHigh (((_grounded,cmds),_nextGrounded) : ys)
      | Flip `elem` IntMap.elems cmds = cmds : loop (not isHigh) ys
      | otherwise = cmds : loop isHigh ys

    replaceWait :: Command -> IntMap Command -> IntMap Command
    replaceWait cmd cmds =
      case find (\(_bid,cmd') -> cmd' == Wait) (IntMap.toList cmds) of 
        Nothing -> undefined
        Just (bid,_) -> IntMap.insert bid cmd cmds

    replaceFlipWithWait :: IntMap Command -> IntMap Command
    replaceFlipWithWait = fmap (\cmd -> if cmd==Flip then Wait else cmd)

    flipOnly :: IntMap Command -> IntMap Command
    flipOnly cmds = IntMap.fromList (zip (IntMap.keys cmds) (Flip : repeat Wait))
