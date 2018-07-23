module TraceOptimizer where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List

import Matrix
import Model
import Sim
import State

optimize :: Model -> Model -> Trace -> Trace
optimize ini fini trace = concat [IntMap.elems cmds | cmds <- loop Low transitions, not (all (== Wait) cmds)]
  where
    transitions :: [(Bool, IntMap Command, Bool)]
    transitions = [(pre, cmds, post) | ((pre,cmds),post) <- zip xs (tail (map fst xs) ++ [True])]
      where
        xs :: [(Bool, IntMap Command)]
        xs = f (initialStateForReassembly ini fini trace)
          where
            f :: SystemState -> [(Bool, IntMap Command)]
            f s
              | IntMap.null (stBots s) = []
              | otherwise = 
                  ( stateIsGrounded s
                  , IntMap.fromAscList $ [(bid, cmd) | ((bid,_),cmd) <- zip (IntMap.toAscList (stBots s)) (take n (stTrace s))]
                  ) : f (execOneStep s)
              where
                n = IntMap.size (stBots s)

    loop :: Harmonics -> [(Bool, IntMap Command, Bool)] -> [IntMap Command]
    loop _ [] = []
    loop Low ((_True, cmds, True) : ts)
      | Flip `elem` IntMap.elems cmds = replaceFlipWithWait cmds : loop Low ts
      | otherwise = cmds : loop Low ts
    loop Low tts@((_True, cmds, False) : ts) =
      case flipInplace cmds of
        Just cmds' -> cmds' : loop High ts
        Nothing -> flipOnly cmds : loop High tts
    loop High tts@((True, cmds, True) : ts) =
      case flipInplace cmds of
        Just cmds' -> cmds' : loop Low ts
        Nothing -> flipOnly cmds : loop Low tts -- 現在がgroundedなので先にlowにできる
    loop High ((False, cmds, True) : ts) =
      case flipInplace cmds of
        Just cmds' -> cmds' : loop Low ts
        Nothing -> cmds : loop High ts -- 現在がgroundedでないので先にlowにすることはできない
    loop harmonics ((_preGrounded,cmds,_postGrounded) : ts)
      | Flip `elem` IntMap.elems cmds = cmds : loop (flipHarmonics harmonics) ts
      | otherwise = cmds : loop harmonics ts

    flipInplace :: IntMap Command -> Maybe (IntMap Command)
    flipInplace cmds
      | Flip `elem` IntMap.elems cmds = return cmds
      | Wait `elem` IntMap.elems cmds = return $ replaceWait Flip cmds
      | otherwise = Nothing

    replaceWait :: Command -> IntMap Command -> IntMap Command
    replaceWait cmd cmds =
      case find (\(_bid,cmd') -> cmd' == Wait) (IntMap.toList cmds) of 
        Nothing -> undefined
        Just (bid,_) -> IntMap.insert bid cmd cmds

    replaceFlipWithWait :: IntMap Command -> IntMap Command
    replaceFlipWithWait = fmap (\cmd -> if cmd==Flip then Wait else cmd)

    flipOnly :: IntMap Command -> IntMap Command
    flipOnly cmds = IntMap.fromList (zip (IntMap.keys cmds) (Flip : repeat Wait))
