{-# OPTIONS_GHC -Wno-missing-fields #-}

module NaiveBot
  ( getAssembleTrace
  , getAssembleTrace'
  , getDisassembleTrace
  , getDisassembleTrace'
  ) where

import Control.Monad (replicateM)
import Control.Monad.State.Lazy
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import Coordinate
import Model
import State


-------------------------------------------------------

data B = B
  { bBot    :: Bot       -- the bot
  , bTrace  :: [Trace]   -- traces of the bot (including fusion's)
  , bRange  :: (Int,Int) -- x-axis [a,b) in charge.
  }

type BState a = State B a

-------------------------------------------------------

sMoveDx :: Int -> BState ()
sMoveDx 0 = return ()
sMoveDx n = do
  modify f
  where
    f :: B -> B
    f b@B{ bBot   = bot@Bot{botPos=Coord(x,y,z)}
         , bTrace = bTrace
         } =
      b{ bBot   = bot{botPos=Coord(x+n,y,z)}
       , bTrace = [SMove (n,0,0)]:bTrace
       }

sMoveDy :: Int -> BState ()
sMoveDy 0 = return ()
sMoveDy n = do
  modify f
  where
    f :: B -> B
    f b@B{ bBot   = bot@Bot{botPos=Coord(x,y,z)}
         , bTrace = bTrace
         } = 
      b{ bBot   = bot{botPos=Coord(x,y+n,z)}
       , bTrace = [SMove (0,n,0)]:bTrace
       }

sMoveDz:: Int -> BState ()
sMoveDz 0 = return ()
sMoveDz n = do
  modify f
  where
    f :: B -> B
    f b@B{ bBot   = bot@Bot{botPos=Coord(x,y,z)}
         , bTrace = bTrace
         } =
      b{ bBot   = bot{botPos=Coord(x,y,z+n)}
       , bTrace = [SMove (0,0,n)]:bTrace
       }

-- absolute coord
sMoveAbs :: (Int,Int,Int) -> BState ()
sMoveAbs (x,y,z) = do
  B{bBot=Bot{botPos=Coord(cx,cy,cz)}} <- get
  let (qx,rx) = (x-cx)`quotRem`15
  replicateM (abs qx) (sMoveDx (15*(signum qx)))
  sMoveDx rx
  let (qy,ry) = (y-cy)`quotRem`15
  replicateM (abs qy) (sMoveDy (15*(signum qy)))
  sMoveDy ry
  let (qz,rz) = (z-cz)`quotRem`15
  replicateM (abs qz) (sMoveDz (15*(signum qz)))
  sMoveDz rz


fillBottom :: BState ()
fillBottom = do
  modify f
  where
    f :: B -> B
    f b@B{ bTrace = bTrace} =
      b{ bTrace = [Fill (0,-1,0)]:bTrace }

voidBottom :: BState ()
voidBottom = do
  modify f
  where
    f :: B -> B
    f b@B{ bTrace = bTrace} =
      b{ bTrace = [Void (0,-1,0)]:bTrace}

fissionX :: BState B
fissionX = do
  modify f
  B{bBot=Bot{botId=botId,botPos=Coord(x,y,z)}, bTrace=bTrace, bRange=(ra,rb)} <- get
  return $ B{ bBot    = Bot{botId=botId+1, botPos=Coord(x+1,y,z), botSeeds=IntSet.empty}
            , bTrace  = take (length bTrace) (repeat [])
            , bRange  = (rb, rb+rb-ra)
            }
  where
    f :: B -> B
    f b@B{ bBot   = Bot{botId=botId}
         , bTrace = bTrace
         } =
      b{ bTrace = [Fission (1,0,0) (39-botId)]:bTrace}

fusion :: B -> BState ()
fusion B{bBot=Bot{botPos=Coord(x',y',z')}, bTrace=bTrace'} = do
  sMoveAbs (x'-1,y',z')
  B{bTrace=bTrace} <- get
  let len = max (length bTrace) (length bTrace')
      merged = reverse (take len (zipWith (++) (reverse bTrace ++ repeat[Wait]) (reverse bTrace' ++ repeat[Wait])))
  modify (f merged)
  where
    f :: [Trace] -> B -> B
    f merged b@B{bTrace = bTrace} =
      b{bTrace = [FusionP (1,0,0), FusionS (-1,0,0)]:merged}

cFlip :: BState ()
cFlip = do
  modify f
  where
    f :: B -> B
    f b@B{ bTrace = bTrace } =
      b{ bTrace = [Flip]:bTrace}

cHalt :: BState ()
cHalt = do
  modify f
  where
    f :: B -> B
    f b@B{ bTrace = bTrace} = 
      b{ bTrace = [Halt]:bTrace}

-------------------------------------------------------

getTrace :: (Model -> BState ()) -> Model -> [Trace]
getTrace bot md@(Model r _mat) =
  reverse $ bTrace $ execState (bot md)
  $ B{ bBot   = Bot{botPos=Coord(0,0,0), botId=1, botSeeds=IntSet.empty}
     , bTrace = []
     , bRange = (0, q+(if rest > 0 then 1 else 0))
     }
  where
    (q,rest) = r`divMod`40 -- max 40 parallel

getAssembleTrace :: Model -> Trace
getAssembleTrace =
  concat . getAssembleTrace'

getAssembleTrace' :: Model -> [Trace]
getAssembleTrace' =
  getTrace assembleRoot

getDisassembleTrace :: Model -> Trace
getDisassembleTrace =
  concat . getDisassembleTrace'

getDisassembleTrace' :: Model -> [Trace]
getDisassembleTrace' =
  getTrace disassembleRoot


-------------------------------------------------------

assembleRoot :: Model -> BState ()
assembleRoot md@(Model r _mat) = do
  cFlip
  parallelAssemble md
  cFlip
  sMoveAbs (0,r-1,0)
  sMoveAbs (0,0,0)
  cHalt

disassembleRoot:: Model -> BState ()
disassembleRoot md@(Model r _mat) = do
  sMoveAbs (0,r-1,0)
  cFlip
  parallelDisassemble md
  cFlip
  sMoveAbs (0,0,0)
  cHalt

parallelAssemble :: Model -> BState ()
parallelAssemble md@(Model r mat) = do
  B{bRange=(ra,rb)} <- get
  case rb > r-1 of
    True -> do
      assemble md
    _    -> do
      sMoveAbs (rb-1,0,0)
      b' <- fissionX
      let b'' = execState (parallelAssemble md) b'
      assemble md
      fusion b''
      sMoveAbs (ra,r-1,0)

parallelDisassemble :: Model -> BState ()
parallelDisassemble md@(Model r mat) = do
  B{bRange=(ra,rb)} <- get
  case rb > r-1 of
    True -> do
      disassemble md
    _    -> do
      sMoveAbs (rb-1,r-1,0)
      b' <- fissionX
      let b'' = execState (parallelDisassemble md) b'
      disassemble md
      fusion b''
      sMoveAbs (ra,0,0)

assemble :: Model -> BState ()
assemble (Model r mat) = do
  B{bRange=(ra,rb)} <- get
  sequence_
    [ fillFloor i (maybe [] (filter (\(x,z)->ra<=x&&x<rb) . Set.toList) mset)
    | i<-[0..r-2]
    , let mset = IntMap.lookup i mat
    ]
  sMoveAbs (ra,r-1,0)

disassemble :: Model -> BState ()
disassemble (Model r mat) = do
  B{bRange=(ra,rb)} <- get
  sequence_
    [ voidFloor i (maybe [] (filter (\(x,z)->ra<=x&&x<rb) . Set.toList) mset)
    | i<-[r-1,r-2..0]
    , let mset = IntMap.lookup i mat
    ]
  sMoveAbs (ra,0,0)

fillFloor :: Int -> [(Int,Int)] -> BState ()
fillFloor fno xs =
  mapM_ f xs
  where
    f :: (Int,Int) -> BState ()
    f (x,z) = do
      sMoveAbs (x,fno+1,z)
      fillBottom

voidFloor :: Int -> [(Int,Int)] -> BState ()
voidFloor fno xs =
  mapM_ f xs
  where
    f :: (Int,Int) -> BState ()
    f (x,z) = do
      sMoveAbs (x,fno+1,z)
      voidBottom

