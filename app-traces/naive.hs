import Control.Applicative ((*>))
import Control.Monad (forever, unless, void)
import Control.Concurrent (forkIO, getNumCapabilities)
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

problemsDir :: FilePath
problemsDir = "/home/icfpc2018-data/problems/F"

defaultTraceDir :: FilePath
defaultTraceDir = "/home/icfpc2018-data/default/F"

run :: (String -> IO ())
    -> FilePath
    -> FilePath
    -> IO ()
run putLog dst sf = do
  let df = stripSuffix_ tgtSuf sf <.> "nbt"
      label = sf ++ " --> " ++ df
  putLog $ "running: " ++ label
  runNaive (problemsDir </> sf) (dst </> df)
  putLog $ "done   : " ++ label

runAll :: FilePath -> IO ()
runAll dst = do
  do e <- doesDirectoryExist dst
     unless e . fail $ dst ++ " does not exist!"
  putStrLn "copyng default FD*.nbt FR*.nbt files ..."
  let dtr = defaultTraceDir
  (ioExitCode =<<) . system $ unwords ["cp", "-a", dtr </> "FD*.nbt", dtr </> "FR*.nbt", dst ++ "/"]
  putStrLn "processing FA* files ..."
  let large f = any (`isPrefixOf` f)
                [ "FA186" --- , "FA185", "FA184", "FA183", "FA182", "FA181", "FA180"
                , "FA178", "FA175", "FA173"]
  sfs0 <- filter (tgtSuf `isSuffixOf`) . filter ("FA" `isPrefixOf`) <$> getChilds problemsDir
  let ls = filter large         sfs0
      ns = filter (not . large) sfs0
      sfs = ls ++ ns  -- enqueue larges first
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
