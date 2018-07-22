{-# OPTIONS_GHC -Wno-missing-fields #-}

module NaiveBot where

import Control.Monad (replicateM)
import qualified Control.Monad.State.Lazy as St
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import Coordinate
import Model
import State

type OgaBot = (Bot, Trace)
type OgaBotSt a = St.State OgaBot a

sMoveDx :: Int -> OgaBotSt ()
sMoveDx 0 = return ()
sMoveDx n = do
  St.modify f
  where
    f :: OgaBot -> OgaBot
    f (bot@Bot{botPos=Coord(x,y,z)}, trs) =
      (bot{botPos=Coord(x+n,y,z)}, SMove (n,0,0):trs)

sMoveDy :: Int -> OgaBotSt ()
sMoveDy 0 = return ()
sMoveDy n = do
  St.modify f
  where
    f :: OgaBot -> OgaBot
    f (bot@Bot{botPos=Coord(x,y,z)}, trs) =
      (bot{botPos=Coord(x,y+n,z)}, SMove (0,n,0):trs)

sMoveDz:: Int -> OgaBotSt ()
sMoveDz 0 = return ()
sMoveDz n = do
  St.modify f
  where
    f :: OgaBot -> OgaBot
    f (bot@Bot{botPos=Coord(x,y,z)}, trs) =
      (bot{botPos=Coord(x,y,z+n)}, SMove (0,0,n):trs)

-- absolute coord
sMoveAbs :: (Int,Int,Int) -> OgaBotSt ()
sMoveAbs (x,y,z) = do
  (Bot{botPos=Coord(cx,cy,cz)},trs) <- St.get
  let (qx,rx) = (x-cx)`quotRem`15
  replicateM (abs qx) (sMoveDx (15*(signum qx)))
  sMoveDx rx
  let (qy,ry) = (y-cy)`quotRem`15
  replicateM (abs qy) (sMoveDy (15*(signum qy)))
  sMoveDy ry
  let (qz,rz) = (z-cz)`quotRem`15
  replicateM (abs qz) (sMoveDz (15*(signum qz)))
  sMoveDz rz


fillBottom :: OgaBotSt ()
fillBottom = do
  St.modify f
  where
    f :: OgaBot -> OgaBot
    f (bot, trs) =
      (bot, Fill (0,-1,0):trs)

cFlip :: OgaBotSt ()
cFlip = do
  St.modify f
  where
    f :: OgaBot -> OgaBot
    f (bot, trs) =
      (bot, Flip:trs)


cHalt :: OgaBotSt ()
cHalt = do
  St.modify f
  where
    f :: OgaBot -> OgaBot
    f (bot, trs) =
      (bot, Halt:trs)

-------------------------------------------------------

getAssembleTrace :: Model -> Trace
getAssembleTrace m =
  reverse $ snd $ St.execState (naiveBot m) (Bot{botPos=Coord(0,0,0),botId=1,botSeeds=IntSet.empty}, [])

naiveBot :: Model -> OgaBotSt ()
naiveBot (Model r mtx) = do
  cFlip
  sequence_
    [ fillFloor i (maybe [] Set.toList mset)
    | i<-[0..r-2]
    , let mset = IntMap.lookup i mtx
    ]
  cFlip
  sMoveAbs (r-1,r-1,r-1)
  sMoveAbs (r-1,0,r-1)
  sMoveAbs (r-1,0,0)
  sMoveAbs (0,0,0)
  cHalt

fillFloor :: Int -> [(Int,Int)] -> OgaBotSt ()
fillFloor fno xs =
  sequence_ (sMoveDy 1 : map (fillFloor1 fno) xs)

fillFloor1 :: Int -> (Int,Int) -> OgaBotSt ()
fillFloor1 y (x,z) = do
  sMoveAbs (x,y+1,z)
  fillBottom

    
