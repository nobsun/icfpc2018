{-# OPTIONS_GHC -Wno-missing-fields #-}

module NaiveBot
  ( getAssembleTrace
  , getAssembleTrace'
  , getDisassembleTrace
  , getDisassembleTrace'
  , getReassembleTrace
  , getReassembleTrace'
  ) where

import Control.Monad (replicateM)
import Control.Monad.State.Lazy
import qualified Data.Foldable as Fold
import Data.List (sort, sortBy)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Coordinate
import Model
import State

-------------------------------------------------------
-- x軸を40分割してボットに割り当てる戦略.
-- 自身のx軸の領域内を動くので, 他のボットと競合しない.
-- 分裂するときは, 分裂先が横のボットの領域内になるようにし
-- 逆に融合するときも, 横のボットと領域の境界越しに並ぶようにする.
-------------------------------------------------------

data B = B
  { bBot    :: Bot       -- the bot
  , bTrace  :: Seq.Seq (Seq.Seq Command)   -- ボットのコマンドリスト. n番目の要素はnステップ時のfusion済みボットのコマンドリストが入る
  , bRange  :: (Int,Int) -- ボットが担当するx軸の範囲 [a,b)
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
       , bTrace = bTrace Seq.|> Seq.fromList[SMove (n,0,0)]
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
       , bTrace = bTrace Seq.|> Seq.fromList[SMove (0,n,0)]
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
       , bTrace = bTrace Seq.|> Seq.fromList[SMove (0,0,n)]
       }

-- 絶対座標で移動先を指定する.
-- x y z の順で直線移動するので, その実装が影響する可能性はある
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

cFill :: ND -> BState ()
cFill nd = do
  modify f
  where
    f :: B -> B
    f b@B{ bTrace = bTrace} =
      b{ bTrace = bTrace Seq.|> Seq.fromList[Fill nd] }

cVoid :: ND -> BState ()
cVoid nd = do
  modify f
  where
    f :: B -> B
    f b@B{ bTrace = bTrace} =
      b{ bTrace = bTrace Seq.|> Seq.fromList[Void nd] }

-- (1,0,0)固定でボットを生成する.
fissionX :: BState B
fissionX = do
  modify f
  B{bBot=Bot{botId=botId,botPos=Coord(x,y,z)}, bTrace=bTrace, bRange=(ra,rb)} <- get
  return $ B{ bBot    = Bot{botId=botId+1, botPos=Coord(x+1,y,z), botSeeds=IntSet.empty} --seedの状態は管理していない
            -- bTraceのn番目の要素はnステップ時のコマンドリストなので, 生成される前の時刻分は空リストで埋めておく
            -- (fusion時にマージできるように)
            , bTrace  = Seq.iterateN (Seq.length bTrace) id Seq.empty
            , bRange  = (rb, rb+rb-ra)
            }
  where
    f :: B -> B
    f b@B{ bBot   = Bot{botId=botId}
         , bTrace = bTrace
         } =
      b{ bTrace = bTrace Seq.|> Seq.fromList[Fission (1,0,0) (39-botId)] }

fusion :: B -> BState ()
fusion B{bBot=Bot{botPos=Coord(x',y',z')}, bTrace=bTrace'} = do
  sMoveAbs (x'-1,y',z') -- fusionするためにボットの横(x軸)へ移動する
  B{bTrace=bTrace} <- get
  let len    = max 0 (Seq.length bTrace' - Seq.length bTrace)
      len'   = max 0 (Seq.length bTrace - Seq.length bTrace')
      -- fusionのタイミングでコマンドリストをマージする
      -- トレースが長いほうにあわせるために, 短いほうはWaitをいれておく
      merged = Seq.zipWith (Seq.><) (bTrace Seq.>< Seq.iterateN len id (Seq.singleton Wait)) (bTrace' Seq.>< Seq.iterateN len' id (Seq.singleton Wait))
  modify (f merged)
  where
    f :: Seq.Seq (Seq.Seq Command) -> B -> B
    f merged b@B{bTrace = bTrace} =
      b{bTrace = merged Seq.|> Seq.fromList[FusionP (1,0,0), FusionS (-1,0,0)]}

cFlip :: BState ()
cFlip = do
  modify f
  where
    f :: B -> B
    f b@B{ bTrace = bTrace } =
      b{ bTrace = bTrace Seq.|> Seq.fromList[Flip]}

cHalt :: BState ()
cHalt = do
  modify f
  where
    f :: B -> B
    f b@B{ bTrace = bTrace} = 
      b{ bTrace = bTrace Seq.|> Seq.fromList[Halt]}

-------------------------------------------------------

getTrace :: (Model -> BState ()) -> Model -> Seq.Seq (Seq.Seq Command)
getTrace bot md@(Model r _mat) =
  bTrace $ execState (bot md)
  $ B{ bBot   = Bot{botPos=Coord(0,0,0), botId=1, botSeeds=IntSet.empty}
     , bTrace = Seq.empty
     , bRange = (0, q+(if rest > 0 then 1 else 0))
     }
  where
    (q,rest) = r`divMod`40 -- max 40 parallel

getAssembleTrace :: Model -> Trace
getAssembleTrace =
  Fold.toList . Fold.msum . getAssembleTrace'

getAssembleTrace' :: Model -> Seq.Seq (Seq.Seq Command)
getAssembleTrace' =
  getTrace assembleRoot

getDisassembleTrace :: Model -> Trace
getDisassembleTrace =
  Fold.toList . Fold.msum . getDisassembleTrace'

getDisassembleTrace' :: Model -> Seq.Seq (Seq.Seq Command)
getDisassembleTrace' =
  getTrace disassembleRoot

getReassembleTrace :: Model -> Model -> Trace
getReassembleTrace src tgt =
  Fold.toList (Fold.msum (getReassembleTrace' src tgt))

getReassembleTrace' :: Model -> Model -> Seq.Seq (Seq.Seq Command)
getReassembleTrace' src@(Model r _) tgt =
  bTrace $ execState (reassembleRoot src tgt)
  $ B{ bBot   = Bot{botPos=Coord(0,0,0), botId=1, botSeeds=IntSet.empty}
     , bTrace = Seq.empty
     , bRange = (0, q+(if rest > 0 then 1 else 0))
     }
  where
    (q,rest) = r`divMod`40 -- max 40 parallel

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

reassembleRoot:: Model -> Model -> BState ()
reassembleRoot src@(Model r _) tgt = do
  cFlip
  parallelReassemble src tgt
  cFlip
  sMoveAbs (0,r-1,0)
  sMoveAbs (0,0,0)
  cHalt

parallelAssemble :: Model -> BState ()
parallelAssemble md@(Model r _) = do
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
parallelDisassemble md@(Model r _) = do
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

parallelReassemble :: Model -> Model -> BState ()
parallelReassemble src@(Model r _) tgt = do
  B{bRange=(ra,rb)} <- get
  case rb > r-1 of
    True -> do
      reassemble src tgt
    _    -> do
      sMoveAbs (rb-1,0,0) -- 領域の端まで移動して
      b' <- fissionX      -- 横の領域にボットを生成する
      let b'' = execState (parallelReassemble src tgt) b'
      reassemble src tgt
      fusion b''          -- fusionするときにトレースもマージする
      sMoveAbs (ra,r-1,0) -- 領域の端っこでfusionされるのを待つ

assemble :: Model -> BState ()
assemble (Model r mat) = do
  B{bRange=(ra,rb)} <- get
  sequence_
    [ fillFloor (x,y,z) 
    | y <- [0..r-2]
    , let mset = IntMap.lookup y mat
    , (x,z) <- maybe [] (filter (\(x,z)->ra<=x&&x<rb) . Set.toList) mset
    ]
  sMoveAbs (ra,r-1,0) -- 領域の端っこでfusionされるのを待つ

disassemble :: Model -> BState ()
disassemble (Model r mat) = do
  B{bRange=(ra,rb)} <- get
  sequence_
    [ voidFloor (x,y,z)
    | y <- [r-1,r-2..0]
    , let mset = IntMap.lookup y mat
    , (x,z) <- maybe [] (filter (\(x,z)->ra<=x&&x<rb) . Set.toList) mset
    ]
  sMoveAbs (ra,0,0) -- 領域の端っこでfusionされるのを待つ

reassemble :: Model -> Model -> BState ()
reassemble (Model r srcmat) (Model _ tgtmat) = do
  B{bRange=(ra,rb)} <- get
  -- とにかくz軸方向を前後しながら動くように順番を調整する
  let ls = zip (concat (repeat [True,False]))
        [ (x, y)
        | y <- [0..r-2]
        , x <- [ra..min (rb-1) (r-2)]
        ]
  sequence_ $ concat
    [ [ sMoveAbs (x,y, if dir then 0 else r-1)] ++ -- z軸の端に移動する正規化(無駄な動きをするが分かりやすいので)
      [ reconst ty dir (x,y,z)
      -- sortの順番を (z, dir) の順で判定させる (dir=direction)
      -- tgt のほう srcのあとに持ってきたいので z座標をずらしている
      | ((_x,z),ty) <- orderBy dir $ zip srcs (repeat (not dir, 'S')) ++ zip (map (adjustz dir) tgts) (repeat (dir, 'T'))
      ]
      ++ [sMoveAbs (x,y, if dir then r-1 else 0)] -- z軸のもう一方の端に移動する正規化(同上)
    | (dir, (x, y)) <- ls
    , let srcs = maybe [] (filter ((==x).fst) . Set.toList) (IntMap.lookup y srcmat)
    , let tgts = maybe [] (filter ((==x).fst) . Set.toList) (IntMap.lookup y tgtmat)
    ]
  sMoveAbs (ra,r-1,0) -- 領域の端っこでfusionされるのを待つ
  where
    orderBy True  = sort
    orderBy False = sortBy (flip compare)

    adjustz True  (i,j) = (i,j+1)
    adjustz False (i,j) = (i,j-1)

fillFloor :: (Int,Int,Int) -> BState ()
fillFloor (x,y,z) = do
  sMoveAbs (x,y+1,z) -- 生成するブロックの上に移動
  cFill (0,-1,0)     -- 自分の真下に生成

voidFloor :: (Int,Int,Int) -> BState ()
voidFloor (x,y,z) = do
  sMoveAbs (x,y+1,z) -- 壊すブロックの上に移動
  cVoid (0,-1,0)     -- 自分の真下を壊す

reconst :: (Bool, Char) -> Bool -> (Int,Int,Int) -> BState ()
reconst (_,'S') dir (x,y,z) = do
  -- S はsrcなので壊すべき対象
  sMoveAbs (x,y,z+dz)
  cVoid (0,0,-dz) -- ボットが進んでいる方向(z軸)正面のブロックを壊す
  where
    dz = if dir then -1 else 1
reconst (_,'T') dir (x,y,z) = do -- zは調整済みなので注意
  -- T はtgtなので生成すべき対象
  sMoveAbs (x,y,z)
  cFill (0,0,dz) -- ボットが進んでる方向(z軸)と逆(おしり)に生成する
  where
    dz = if dir then -1 else 1


