module TraceInverser
  ( inverse
  )where

import Control.Monad.State.Lazy

import Coordinate
import State


data T = T
  { source   :: Trace -- 元のトレース
  , nextStep :: Trace -- 1ステップでの並列ボットのコマンド列
  , inversed :: Trace -- 変換済みトレース
  , bots     :: Int   -- 現在のボット数
  }


-- naive-bot専用のFA->FD変換
inverse :: Trace -> Trace
inverse trs =
  reverse $ inversed $ execState inverseAll initT
  where
    -- Haltの位置だけ付け替える
    initT = T{source=tail (reverse (Halt:trs)), nextStep=[], inversed=[], bots=1}

----------------------------------------------------------

isEnd :: State T Bool
isEnd = do
  t@T{source=ss, nextStep=ns, inversed=is, bots=b} <- get
  return (null ns && null ss)

getCommand :: State T Command
getCommand = do
  t@T{source=ss, nextStep=ns, inversed=is, bots=b} <- get
  case (ns,ss) of
    -- Fusionがきたら先回りしてbを増やしておく
    ([],FusionS _:_) -> do
      let (aas,bs) = splitAt (b+1) ss
          (a:as)   = reverse aas
      put t{source=bs, nextStep=as}
      return a
    -- Fissionがきたら先回りしてbを減らしておく
    ([],Fission _ _:_) -> do
      let (aas,bs) = splitAt (b-1) ss
          (a:as)   = reverse aas
      put t{source=bs, nextStep=as}
      return a
    ([],_) -> do
      let (aas,bs) = splitAt b ss --ボット数だけコマンド列をとる
          (a:as)   = reverse aas  --逆向きに読んでるので, 再度逆向きに直す
      put t{source=bs, nextStep=as}
      return a
    (a:as,_) -> do
      put t{nextStep=as}
      return a


putInverse :: Command -> State T ()
putInverse c = do
  t@T{source=ss, nextStep=ns, inversed=is, bots=b} <- get
  put t{inversed=c:is}

addBot :: State T ()
addBot = do
  t@T{source=ss, nextStep=ns, inversed=is, bots=b} <- get
  put t{bots=b+1}

removeBot :: State T ()
removeBot = do
  t@T{source=ss, nextStep=ns, inversed=is, bots=b} <- get
  put t{bots=b-1}

inverseOne :: State T ()
inverseOne = do
  c <- getCommand
  case c of
    (SMove (dx,dy,dz)) ->
        putInverse (SMove (-dx,-dy,-dz))
    (Fill nd) ->
        putInverse (Void nd)
    (Void nd) ->
        putInverse (Fill nd)
    (FusionP nd) -> do
        (FusionS _) <- getCommand
        T{bots=b} <- get
        -- naive botは Fission時に子ボットにすべてのseedsを渡す!
        putInverse (Fission nd (39-b))
        addBot
    (Fission nd m) -> do
        -- naive botは Fission (1,0,0) しかしてない!
        putInverse (FusionP (1,0,0) )
        putInverse (FusionS (-1,0,0) )
        removeBot
    _ ->
        -- そのほかは何もしない
        putInverse c

inverseAll :: State T ()
inverseAll = do
  end <- isEnd
  if end then return () else inverseOne >> inverseAll

