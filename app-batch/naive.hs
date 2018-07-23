import Control.Monad (when, unless)
import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (deepseq)
import System.Timeout (timeout)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist)

import Concurrent (concurrentIO, newLog)
import qualified Path
import ProblemSet (ProblemFile, runProblemFile)
import qualified ProblemSet as ProbSet
import TraceEncoder (writeTraceFile)
import Model (readModel)
import qualified Model
import NaiveBot (getAssembleTrace, getDisassembleTrace, getReassembleTrace)
import TraceOptimizer (optimize)
import TraceInverser (inverse)


data Mode
  = NoOpt
  | Opt
  | TOpt
  | Inv
  deriving (Eq, Show)

descMode :: Mode -> String
descMode NoOpt = "no-optimize"
descMode Opt   = "optimize"
descMode TOpt  = "optimize (may timeoute)"
descMode Inv  =  "optimize (may timeoute, inversed disassemble)"

limitAD :: Int
limitAD = 4 * 60 * 1000 * 1000

-- limitR :: Int
-- limitR = 6 * 60 * 1000 * 1000

assemble :: Mode -> FilePath -> Int -> FilePath -> IO Bool
assemble mode nbt _n tgt_ = do
  tgt <- readModel $ Path.problems </> tgt_
  let trs0 = getAssembleTrace tgt
      optimized = optimize (Model.emptyR tgt) tgt trs0
      trs NoOpt  =  return (trs0,      False)
      trs Opt    =  return (optimized, False)
      trs TOpt   =  maybe (trs0, True) (\x -> (x, False)) <$> timeout limitAD (optimized `deepseq` return optimized)
      trs Inv    =  maybe (trs0, True) (\x -> (x, False)) <$> timeout limitAD (optimized `deepseq` return optimized)
  (t, tout) <- trs mode
  writeTraceFile nbt t
  return tout

disassemble :: Mode -> FilePath -> Int -> FilePath -> IO Bool
disassemble _mode nbt _n src_ = do
  src <- readModel $ Path.problems </> src_
  let trs0 = getDisassembleTrace src
  writeTraceFile nbt trs0
  return False

disassembleInv :: FilePath -> Int -> FilePath -> IO Bool
disassembleInv nbt _n src_ = do
  tgt <- readModel $ Path.problems </> src_
  let trs0 = getAssembleTrace tgt  -- run as assemble in inverse mode
      optimized = inverse $ optimize (Model.emptyR tgt) tgt trs0
      trs       =  maybe (inverse trs0, True) (\x -> (x, False)) <$> timeout limitAD (optimized `deepseq` return optimized)
  (t, tout) <- trs
  writeTraceFile nbt t
  return tout

reassemble :: Mode -> FilePath -> Int -> FilePath -> FilePath -> IO Bool
reassemble _mode nbt _n src_ tgt_ = do
  src <- readModel $ Path.problems </> src_
  tgt <- readModel $ Path.problems </> tgt_
  let trs0 = getReassembleTrace src tgt
  writeTraceFile nbt trs0
  return False

reassembleO :: Mode -> FilePath -> Int -> FilePath -> FilePath -> IO Bool
reassembleO mode nbt _n src_ tgt_ = do
  src <- readModel $ Path.problems </> src_
  tgt <- readModel $ Path.problems </> tgt_
  let trs0 = getReassembleTrace src tgt
      optimized = optimize (Model.emptyR tgt) tgt trs0
      trs NoOpt  =  return (trs0,      False)
      trs Opt    =  return (optimized, False)
      trs TOpt   =  maybe (trs0, True) (\x -> (x, False)) <$> timeout limitAD (optimized `deepseq` return optimized)
      trs Inv    =  maybe (trs0, True) (\x -> (x, False)) <$> timeout limitAD (optimized `deepseq` return optimized)
  (t, tout) <- trs mode
  writeTraceFile nbt t
  return tout

run :: (String -> IO ())
    -> FilePath
    -> Mode
    -> ProblemFile
    -> IO ()
run putLog dst mode pf = do
  let nbtPath = dst </> ProbSet.traceFile pf
      label = nbtPath
      disas Inv   = disassembleInv
      disas m     = disassemble m
  putLog $ "running: " ++ label
  tout <- runProblemFile
    (assemble    mode nbtPath)
    (disas       mode nbtPath)
    (reassembleO mode nbtPath)
    pf
  when tout . putLog $ "timeout: " ++ label
  putLog $ "done   : " ++ label

runAll :: FilePath -> Mode -> IO ()
runAll dst mode = do
  putStrLn $ "dest-dir: " ++ dst
  putStrLn $ "mode: " ++ descMode mode
  do e <- doesDirectoryExist dst
     unless e . fail $ dst ++ " does not exist!"
  putStrLn "processing FA*, FD*, FR* files ..."
  putLog <- newLog
  n <- getNumCapabilities
  let orderedP NoOpt  =  ProbSet.problemsL
      orderedP Opt    =  reverse ProbSet.problemsL  -- opt は高コストなので小さい順
      orderedP TOpt   =  reverse ProbSet.problemsL
      orderedP Inv    =  reverse ProbSet.problemsL
  concurrentIO (n - 1) $ map (run putLog dst mode) $ orderedP mode

main :: IO ()
main = do
  args <- getArgs
  (dst, doOpt) <- case args of
    d:"opt":_   ->  return (d, Opt)
    d:"topt":_  ->  return (d, TOpt)
    d:"inv":_   ->  return (d, Inv)
    d:_         ->  return (d, NoOpt)
    _   -> fail "DEST_DIRECTORY is required."
  runAll dst doOpt
