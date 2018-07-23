{-# OPTIONS -Wall #-}

module ProblemSet (
  problems, problemsL, assembles, disassembles, reassembles,
  traceFile, filePrefix, number,
  ProblemFile (..), runProblemFile,
  ) where

import Data.List (transpose)
import System.FilePath ((<.>))
import Text.Printf (printf)

-----

problems :: [ProblemFile]
problems = assembles ++ disassembles ++ reassembles

-- | large first problems
problemsL :: [ProblemFile]
problemsL =
  concat . transpose
  $ map reverse [reassembles, assembles, disassembles]

assembles :: [ProblemFile]
assembles = map assemble [1 .. 186]

disassembles :: [ProblemFile]
disassembles = map disassemble [1 .. 186]

reassembles :: [ProblemFile]
reassembles = map reassemble [1 .. 115]

-----

data ProblemFile
  = AssembleFile    Int                    FilePath {- tgt -}
  | DisassembleFile Int FilePath {- src -}
  | ReassembleFile  Int FilePath {- src -} FilePath {- tgt -}
  deriving (Eq, Show)

runProblemFile :: (Int -> FilePath -> a)
               -> (Int -> FilePath -> a)
               -> (Int -> FilePath -> FilePath -> a)
               -> ProblemFile
               -> a
runProblemFile a d r pf = case pf of
  AssembleFile    i     tgt  -> a i tgt
  DisassembleFile i src      -> d i src
  ReassembleFile  i src tgt  -> r i src tgt

assemble :: Int -> ProblemFile
assemble n = AssembleFile n $ printf "FA%03d_tgt" n <.> "mdl"

disassemble :: Int -> ProblemFile
disassemble n = DisassembleFile n $ printf "FD%03d_src" n <.> "mdl"

reassemble :: Int -> ProblemFile
reassemble n = ReassembleFile n src tgt
  where
    src = printf "FR%03d_src" n <.> "mdl"
    tgt = printf "FR%03d_tgt" n <.> "mdl"

traceFile :: ProblemFile -> FilePath
traceFile pf = filePrefix pf ++ printf "%03d" (number pf) <.> "nbt"

filePrefix :: ProblemFile -> String
filePrefix =
  runProblemFile
  (\_ _ -> "FA")
  (\_ _ -> "FD")
  (\_ _ _ -> "FR")

number :: ProblemFile -> Int
number = runProblemFile (\n _ -> n) (\n _ -> n) (\n _ _ -> n)
