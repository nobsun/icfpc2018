import Control.Applicative ((*>))
import Control.Monad (forever, unless, void)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Data.List (isPrefixOf, isSuffixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.Process (system)
import System.Exit (ExitCode (..))


import TraceEncoder (writeTraceFile)
import NaiveBot (getAssembleTrace)
import Model (readModel)


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

runNaive :: FilePath -> FilePath -> IO ()
runNaive mdl nbt = do
  model <- readModel mdl
  let trs = getAssembleTrace model
  writeTraceFile nbt trs

stripSuffix_ :: String -> String -> String
stripSuffix_ suf x
  | suf `isSuffixOf` x = take (length x - length suf) x
  | otherwise          = x

tgtSuf :: String
tgtSuf = "_tgt.mdl"

run :: (String -> IO ())
    -> FilePath
    -> FilePath
    -> FilePath
    -> IO ()
run putLog src dst sf = do
  let df = stripSuffix_ tgtSuf sf <.> "nbt"
      label = sf ++ " --> " ++ df
  putLog $ "running: " ++ label
  runNaive (src </> sf) (dst </> df)
  putLog $ "done   : " ++ label

runAll :: FilePath -> FilePath -> FilePath -> IO ()
runAll src dst dtr = do
  do e <- doesDirectoryExist dst
     unless e . fail $ dst ++ " does not exist!"
  putStrLn "copyng default FD*.nbt FR*.nbt files ..."
  (ioExitCode =<<) . system $ unwords ["cp", "-a", dtr </> "FD*.nbt", dtr </> "FR*.nbt", dst ++ "/"]
  putStrLn "processing FA* files ..."
  sfs <- filter (tgtSuf `isSuffixOf`) . filter ("FA" `isPrefixOf`) <$> getChilds src
  putLog <- newLog
  concurrent 10 $ map (run putLog src dst) sfs

main :: IO ()
main = do
  args <- getArgs
  (src, dst, dtr) <- case args of
    s:d:t:_ -> return (s, d, t)
    _       -> fail "SOURCE_DIRECTORY, DEST_DIRECTORY and DEFAULT_TRACE_DIRECTORY are required."
  runAll src dst dtr
