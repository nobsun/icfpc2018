import System.Environment

import Data.Foldable (toList, msum)
import Data.List (sort)
import qualified Data.Sequence as Seq (singleton)

import TraceEncoder (writeTraceFile)
import NaiveBot (getAssembleTrace', getDisassembleTrace', getReassembleTrace')
import Model (Model(..), readModel)
import State (Trace, Command(..))

main :: IO ()
main = do
  (flag:args) <- getArgs
  case flag of
    "FR" -> mapM_ solve2 (split 2 (sort args))
    _    -> mapM_ solve1 args

solve1 :: String -> IO ()
solve1 mdl = do
  model <- readModel mdl
  let trs = method model
  putStrLn ("resolution: " ++ show (mdResolution model))
--  mapM_ print trs
  writeTraceFile (mkNbtName mdl) (toList (msum trs))
  where
    method = 
      case take 2 mdl of
        "FA" -> getAssembleTrace'
        "FD" -> getDisassembleTrace'
        _    -> const (Seq.singleton (Seq.singleton Halt))

solve2 :: [String] -> IO ()
solve2 [src,tgt] = do
  srcModel <- readModel src
  tgtModel <- readModel tgt
  let trs = getReassembleTrace' srcModel tgtModel
  putStrLn ("src=" ++ src ++ " tgt=" ++ tgt)
  putStrLn ("resolution: " ++ show (mdResolution srcModel))
--  mapM_ print trs
  writeTraceFile (mkNbtName src) (toList (msum trs))

mkNbtName :: String -> String
mkNbtName str =
  name ++ ".nbt"
  where
    name = takeWhile (/='_') str

split :: Int -> [a] -> [[a]]
split n [] = []
split n xs =
  as : split n bs
  where
    (as,bs) = splitAt n xs
