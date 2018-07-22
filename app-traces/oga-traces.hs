import Control.Monad (unless)
import Data.List (isPrefixOf, isSuffixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.Process (system)
import System.Exit (ExitCode (..))


import TraceEncoder (writeTraceFile)
import NaiveBot (getAssembleTrace)
import Model (readModel)


getChilds :: FilePath -> IO [FilePath]
getChilds = (filter (`notElem` [".", ".."]) <$>) . getDirectoryContents

run :: FilePath -> FilePath -> IO ()
run mdl nbt = do
  model <- readModel mdl
  let trs = getAssembleTrace model
  writeTraceFile nbt trs

stripSuffix_ :: String -> String -> String
stripSuffix_ suf x
  | suf `isSuffixOf` x = take (length x - length suf) x
  | otherwise          = x

runExitCode :: a -> (Int -> a) -> ExitCode -> a
runExitCode s e ec = case ec of
  ExitSuccess   -> s
  ExitFailure c -> e c

ioExitCode :: ExitCode -> IO ()
ioExitCode = runExitCode (return ()) (fail . ("exited failure with code: " ++) . show)

main :: IO ()
main = do
  args <- getArgs
  (src, dst, dtr) <- case args of
    s:d:t:_ -> return (s, d, t)
    _       -> fail "SOURCE_DIRECTORY, DEST_DIRECTORY and DEFAULT_TRACE_DIRECTORY are required."
  do e <- doesDirectoryExist dst
     unless e . fail $ dst ++ " does not exist!"
  (ioExitCode =<<) . system $ unwords ["cp", "-a", dtr </> "FD*.nbt", dtr </> "FR*.nbt", dst ++ "/"]
  let tgtSuf = "_tgt.mdl"
  sfs <- filter (tgtSuf `isSuffixOf`) . filter ("FA" `isPrefixOf`) <$> getChilds src
  let dfs = map ((<.> "nbt") . stripSuffix_ tgtSuf) sfs
  sequence_ $ zipWith  run (map (src </>) sfs) (map (dst </>) dfs)
