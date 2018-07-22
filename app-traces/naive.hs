import Control.Applicative ((*>))
import Control.Monad (forever, unless, void)
import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Data.List (isPrefixOf, isSuffixOf)
import System.Environment (getArgs)
import System.IO (stdout, BufferMode (LineBuffering), hSetBuffering)
import System.FilePath ((</>), (<.>))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.Process (system)
import System.Exit (ExitCode (..))

import TraceEncoder (writeTraceFile)
import Matrix (makeMatrix)
import Model (Model (mdMatrix), readModel)
import NaiveBot (getAssembleTrace, getDisassembleTrace)
import TraceOptimizer (optimize)


runExitCode :: a -> (Int -> a) -> ExitCode -> a
runExitCode s e ec = case ec of
  ExitSuccess   -> s
  ExitFailure c -> e c

ioExitCode :: ExitCode -> IO ()
ioExitCode = runExitCode (return ()) (fail . ("exited failure with code: " ++) . show)

getChilds :: FilePath -> IO [FilePath]
getChilds = (filter (`notElem` [".", ".."]) <$>) . getDirectoryContents

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

runAssemble :: FilePath -> FilePath -> IO ()
runAssemble mdl nbt = do
  model <- readModel mdl
  let trs0 = getAssembleTrace model
      trs  =  optimize (model { mdMatrix = makeMatrix [] }) model trs0
      -- trs
      --   | n < 115    =  optimize (model { mdMatrix = makeMatrix [] }) model trs0
      --   | otherwise  =  trs0
  writeTraceFile nbt trs

runDisassemble :: FilePath -> FilePath -> IO ()
runDisassemble mdl nbt = do
  model <- readModel mdl
  let trs0 = getDisassembleTrace model
      trs  =  optimize model (model { mdMatrix = makeMatrix [] }) trs0
      -- trs
      --   | n < 115    =  optimize model (model { mdMatrix = makeMatrix [] }) trs0
      --   | otherwise  =  trs0
  writeTraceFile nbt trs

stripSuffix_ :: String -> String -> String
stripSuffix_ suf x
  | suf `isSuffixOf` x = take (length x - length suf) x
  | otherwise          = x

tgtSuf :: String
tgtSuf = "_tgt.mdl"

srcSuf :: String
srcSuf = "_src.mdl"

problemsDir :: FilePath
problemsDir = "/home/icfpc2018-data/problems/F"

defaultTraceDir :: FilePath
defaultTraceDir = "/home/icfpc2018-data/default/F"

run :: (String -> IO ())
    -> FilePath
    -> FilePath
    -> IO ()
run putLog dst sf = do
  (suf, action) <- case take 2 sf of
    "FA"  ->  return (tgtSuf, runAssemble)
    "FD"  ->  return (srcSuf, runDisassemble)
    x     ->  fail $ "unknown type: " ++ x
  let df = stripSuffix_ suf sf <.> "nbt"
      label = sf ++ " --> " ++ df
  putLog $ "running: " ++ label
  action (problemsDir </> sf) (dst </> df)
  putLog $ "done   : " ++ label

runAll :: FilePath -> IO ()
runAll dst = do
  do e <- doesDirectoryExist dst
     unless e . fail $ dst ++ " does not exist!"
  putStrLn "copyng default FR*.nbt files ..."
  let dtr = defaultTraceDir
  (ioExitCode =<<) . system $ unwords ["cp", "-a", dtr </> "FR*.nbt", dst ++ "/"]
  putStrLn "processing FA* files and FD* files ..."
  sfs <- filter ((||) <$> ("FA" `isPrefixOf`) <*> ("FD" `isSuffixOf`)) <$> getChilds problemsDir
  putLog <- newLog
  n <- getNumCapabilities
  concurrent (n - 1) $ map (run putLog dst) sfs

main :: IO ()
main = do
  args <- getArgs
  dst <- case args of
    d:_ -> return d
    _   -> fail "DEST_DIRECTORY is required."
  runAll dst
