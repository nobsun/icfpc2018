import Control.Concurrent (getNumCapabilities)
import System.FilePath ((</>))
import System.Environment (getArgs)

import Concurrent (concurrentIO, newLog)
import qualified Path
import ProblemSet (ProblemFile, runProblemFile)
import qualified ProblemSet as ProbSet
import Model (readModel)
import State
  (SystemState (..),
   initialStateForAssembly,
   initialStateForDisassembly,
   initialStateForReassembly,
   Trace, )
import TraceDecoder (readTraceFile)
import Sim (execAll)


initAssemble :: Trace -> Int -> FilePath -> IO SystemState
initAssemble tr _n tgt_ = do
  tgt <- readModel $ Path.problems </> tgt_
  return $ initialStateForAssembly tgt tr

initDisassemble :: Trace -> Int -> FilePath -> IO SystemState
initDisassemble tr _n src_ = do
  src <- readModel $ Path.problems </> src_
  return $ initialStateForDisassembly src tr

initReassemble :: Trace -> Int -> FilePath -> FilePath -> IO SystemState
initReassemble tr _n src_ tgt_ = do
  src <- readModel $ Path.problems </> src_
  tgt <- readModel $ Path.problems </> tgt_
  return $ initialStateForReassembly src tgt tr

run :: (String -> IO ()) -> FilePath -> ProblemFile -> IO ()
run putLog trd pf = do
  let nbtPath = trd </> ProbSet.traceFile pf
      label = nbtPath
  putLog $ "running: " ++ label
  tr <- readTraceFile nbtPath
  s0 <- runProblemFile
    (initAssemble tr)
    (initDisassemble tr)
    (initReassemble tr)
    pf
  let s1 = execAll s0
      success = stTgtMatrix s0 == stMatrix s1
  putLog $ (if success then "success: " else "failure: ") ++ label
  putLog $ "done   : " ++ label

runAll :: FilePath -> IO ()
runAll trd = do
  putLog <- newLog
  n <- getNumCapabilities
  concurrentIO (n - 1) $ map (run putLog trd) $ ProbSet.assembles

main :: IO ()
main = do
  args <- getArgs
  trd  <- case args of
    d:_  -> return d
    []   -> fail "TRACE_DIRECTORY is required."
  runAll trd
