import Control.Applicative ((*>))
import Control.Monad (forever, unless, void)
import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import System.Environment (getArgs)
import System.IO (stdout, BufferMode (LineBuffering), hSetBuffering)
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist)

import qualified Path
import ProblemSet (ProblemFile, runProblemFile, problems, traceFile)
import TraceEncoder (writeTraceFile)
import Model (readModel)
import qualified Model
import NaiveBot (getAssembleTrace, getDisassembleTrace, getReassembleTrace)
import TraceOptimizer (optimize)


newLog :: IO (String -> IO ())
newLog = do
  hSetBuffering stdout LineBuffering
  c <- newChan
  void . forkIO . forever $ putStrLn =<< readChan c
  return $ writeChan c

concurrent :: Int     -- ^ num of threads
           -> [IO a]  -- ^ tasks
           -> IO ()
concurrent n as = do
  tq <- newChan
  wq <- newChan
  let thread = do
        mayT <- readChan tq
        case mayT of
          Nothing  ->  writeChan wq ()  -- end of thread
          Just t   ->  t *> thread      -- next task
  sequence_ . replicate n $ forkIO thread
  mapM_ (writeChan tq) $ map Just as ++ replicate n Nothing  -- enqueue tasks and end-marks
  sequence_ . replicate n $ readChan wq                      -- waiting finish

assemble :: Bool -> FilePath -> Int -> FilePath -> IO ()
assemble doOpt nbt _n tgt_ = do
  tgt <- readModel $ Path.problems </> tgt_
  let trs0 = getAssembleTrace tgt
      trs
        | doOpt      =  optimize (Model.emptyR tgt) tgt trs0
        | otherwise  =  trs0
  writeTraceFile nbt trs

disassemble :: Bool -> FilePath -> Int -> FilePath -> IO ()
disassemble doOpt nbt _n src_ = do
  src <- readModel $ Path.problems </> src_
  let trs0 = getDisassembleTrace src
      trs
        | doOpt      =  optimize src (Model.emptyR src) trs0
        | otherwise  =  trs0
  writeTraceFile nbt trs

reassemble :: Bool -> FilePath -> Int -> FilePath -> FilePath -> IO ()
reassemble doOpt nbt _n src_ tgt_ = do
  src <- readModel $ Path.problems </> src_
  tgt <- readModel $ Path.problems </> tgt_
  let trs0 = getReassembleTrace src tgt
      trs
        | doOpt      =  optimize src tgt trs0
        | otherwise  =  trs0
  writeTraceFile nbt trs

run :: (String -> IO ())
    -> FilePath
    -> Bool
    -> ProblemFile
    -> IO ()
run putLog dst doOpt pf = do
  let nbtPath = dst </> traceFile pf
      label = nbtPath
  putLog $ "running: " ++ label
  runProblemFile
    (assemble    doOpt nbtPath)
    (disassemble doOpt nbtPath)
    (reassemble  doOpt nbtPath)
    pf
  putLog $ "done   : " ++ label

runAll :: FilePath -> Bool -> IO ()
runAll dst doOpt = do
  putStrLn $ "dest-dir: " ++ dst
  putStrLn $ "mode: " ++ if doOpt then "optimize" else "no-optimize"
  do e <- doesDirectoryExist dst
     unless e . fail $ dst ++ " does not exist!"
  putStrLn "processing FA*, FD*, FR* files ..."
  putLog <- newLog
  n <- getNumCapabilities
  concurrent (n - 1) $ map (run putLog dst doOpt) problems

main :: IO ()
main = do
  args <- getArgs
  (dst, doOpt) <- case args of
    d:"opt":_  ->  return (d, True)
    d:_        ->  return (d, False)
    _   -> fail "DEST_DIRECTORY is required."
  runAll dst doOpt
