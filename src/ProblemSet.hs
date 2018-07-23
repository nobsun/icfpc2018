{-# OPTIONS -Wall #-}

module ProblemSet (
  problems, assembles, disassembles, reassembles,
  filePrefix,
  ProblemFile (..), runProblemFile,
  ) where

import Text.Printf (printf)

-----

problems :: [ProblemFile]
problems = assembles ++ disassembles ++ reassembles

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
assemble n = AssembleFile n $ printf "FA%03d_tgt.mdl" n

disassemble :: Int -> ProblemFile
disassemble n = DisassembleFile n $ printf "FD%03d_src.mdl" n

reassemble :: Int -> ProblemFile
reassemble n = ReassembleFile n src tgt
  where
    src = printf "FR%03d_src.mdl" n
    tgt = printf "FR%03d_tgt.mdl" n

filePrefix :: ProblemFile -> String
filePrefix =
  runProblemFile
  (\_ _ -> "FA")
  (\_ _ -> "FD")
  (\_ _ _ -> "FR")
