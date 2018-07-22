import Control.Monad (unless)
import Data.List (isSuffixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import System.Directory (getDirectoryContents, doesDirectoryExist)


import TraceEncoder (writeTraceFile)
import OgaBot (getOgaBotTrace)
import Model (readModel)


getChilds :: FilePath -> IO [FilePath]
getChilds = (filter (`notElem` [".", ".."]) <$>) . getDirectoryContents

run :: FilePath -> FilePath -> IO ()
run mdl nbt = do
  model <- readModel mdl
  let trs = getOgaBotTrace model
  writeTraceFile nbt trs

stripSuffix_ :: String -> String -> String
stripSuffix_ suf x
  | suf `isSuffixOf` x = take (length x - length suf) x
  | otherwise          = x

main :: IO ()
main = do
  args <- getArgs
  (src, dst) <- case args of
    s:d:_ -> return (s, d)
    _     -> fail "SOURCE_DIRECTORY and DEST_DIRECTORY is required."
  do e <- doesDirectoryExist dst
     unless e . fail $ dst ++ " does not exist!"
  let tgtSuf = "_tgt.mdl"
  sfs <- filter (tgtSuf `isSuffixOf`) <$> getChilds src
  let dfs = map ((<.> "nbt") . stripSuffix_ tgtSuf) sfs
  sequence_ $ zipWith  run (map (src </>) sfs) (map (dst </>) dfs)
