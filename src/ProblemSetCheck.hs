module ProblemSetCheck () where

import Data.List (sort)
import System.Directory (getDirectoryContents)

import qualified Path
import ProblemSet (problems, runProblemFile)


fileList :: [FilePath]
fileList =
  problems >>=
  runProblemFile
  (\_  t -> [t])
  (\_  s -> [s])
  (\_  s  t -> [s, t])

_checkSet :: IO ()
_checkSet = do
  efs <- filter (`notElem` [".", "..", "problemsF.txt"]) <$> getDirectoryContents Path.problems
  if sort efs == sort fileList
    then putStrLn "good."
    else fail "not match."
