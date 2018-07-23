-- import Control.Concurrent (getNumCapabilities)
import Text.Printf (printf)
import System.FilePath ((</>))
import System.IO (stdout, BufferMode (LineBuffering), hSetBuffering)
import System.Timeout (timeout)
import System.Environment (getArgs)

-- import Concurrent (concurrentIO, newLog)
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

  -- printf "Time: %d\n" $ stTime s2
  -- printf "Commands: %d\n" $ stCommands s2
  -- printf "Energy: %d\n" $ stEnergy s2

run :: FilePath -> ProblemFile -> IO (Bool, SystemState)
run trd pf = do
  let tfile = ProbSet.traceFile pf
      nbt   = trd </> tfile
      -- label = tfile
  tr <- readTraceFile nbt
  s0 <- runProblemFile
    (initAssemble tr)
    (initDisassemble tr)
    (initReassemble tr)
    pf
  let s1 = execAll s0
      success = stTgtMatrix s0 == stMatrix s1
  return (success, s1)

runCompare :: FilePath -> FilePath -> ProblemFile -> IO ()
runCompare trd1 trd2 pf = do
  (suc1, s1) <- run trd1 pf
  (suc2, s2) <- run trd2 pf
  let label = ProbSet.traceFile pf
      result suc = if suc then "success" else "failure"
      v1 = stEnergy s1
      v2 = stEnergy s2
      energy
        | v1 > v2   =  show v1 ++ " > " ++ show v2
        | v1 < v2   =  show v1 ++ " < " ++ show v2
        | otherwise =  show v1 ++ " = " ++ show v2
        where
      rate = fromIntegral v2 / fromIntegral v1 :: Double
  (maybe (putStrLn $ unwords [label, "timeout"]) return =<<) . timeout (2 * 60 * 1000 * 1000) .
    putStrLn $ unwords [label, "energy", printf "right/left %2.2f" rate, energy, trd1, result suc1, trd2, result suc2]


runAll :: FilePath -> FilePath -> IO ()
runAll trd1 trd2 = do
  hSetBuffering stdout LineBuffering
  mapM_ (runCompare trd1 trd2) $ ProbSet.reassembles

main :: IO ()
main = do
  args <- getArgs
  (trd1, trd2)  <- case args of
    d1:d2:_  -> return (d1, d2)
    _        -> fail "TRACE_DIRECTORY1 and TRACE_DIRECTORY2 are required."
  runAll trd1 trd2
