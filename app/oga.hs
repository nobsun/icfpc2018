import System.Environment

import Data.List (sort)

import TraceEncoder (writeTraceFile)
import NaiveBot (getAssembleTrace', getDisassembleTrace', getReassembleTrace')
import Model (Model(..), readModel)
import State (Trace, Command(..))

main :: IO ()
main = do
  (flag:args) <- getArgs
  case flag of
    "FR" -> mapM_ solve2 (zip (sort args) (tail (sort args)))
    _    -> mapM_ solve1 args

solve1 :: String -> IO ()
solve1 mdl = do
  model <- readModel mdl
  let trs = method model
--  putStrLn ("resolution: " ++ show (mdResolution model))
--  mapM_ print trs
  writeTraceFile (mkNbtName mdl) (concat trs)
  where
    method = 
      case take 2 mdl of
        "FA" -> getAssembleTrace'
        "FD" -> getDisassembleTrace'
        _    -> const [[Halt]]

solve2 :: (String, String) -> IO ()
solve2 (src,tgt) = do
  srcModel <- readModel src
  tgtModel <- readModel tgt
  let trs = getReassembleTrace' srcModel tgtModel
--  putStrLn ("resolution: " ++ show (mdResolution srcModel))
--  mapM_ print trs
  writeTraceFile (mkNbtName src) (concat trs)

mkNbtName :: String -> String
mkNbtName str =
  name ++ ".nbt"
  where
    name = takeWhile (/='_') str

