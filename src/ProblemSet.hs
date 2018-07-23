{-# OPTIONS -Wall #-}

module ProblemSet (
  problems, assembles, disassembles, reassembles,
  ProblemFile, runProblemFile,

  problems',  assembles', disassembles', reassembles',
  Problem, runProblem,
  ) where

import Text.Printf (printf)

-----

problems :: [ProblemFile]
problems = map problemFile problems'

assembles :: [ProblemFile]
assembles = map problemFile assembles'

disassembles :: [ProblemFile]
disassembles = map problemFile disassembles'

reassembles :: [ProblemFile]
reassembles = map problemFile reassembles'

-----

problems' :: [Problem]
problems' = assembles' ++ disassembles' ++ reassembles'

assembles' :: [Problem]
assembles' = map assembleProblem [1 .. 186]

disassembles' :: [Problem]
disassembles' = map disassembleProblem [1 .. 186]

reassembles' :: [Problem]
reassembles' = map reassembleProblem [1 .. 115]

-----

type ProblemFile = (Int, ProblemFile_)

runProblemFile :: (Int -> FilePath -> a)
               -> (Int -> FilePath -> a)
               -> (Int -> FilePath -> FilePath -> a)
               -> ProblemFile
               -> a
runProblemFile a d r (i, pf) =
  runProblemFile_ (a i) (d i) (r i) pf

problemFile :: Problem -> ProblemFile
problemFile = runProblem assemble disassemble reassemble

assemble :: Int -> ProblemFile
assemble n = (n, AssembleFile $ printf "FA%03d_tgt.mdl" n)

disassemble :: Int -> ProblemFile
disassemble n = (n, DisassembleFile $ printf "FD%03d_src.mdl" n)

reassemble :: Int -> ProblemFile
reassemble n = (n, ReassembleFile src tgt)
  where
    src = printf "FR%03d_src.mdl" n
    tgt = printf "FR%03d_tgt.mdl" n

---

data ProblemFile_
  = AssembleFile                       FilePath {- tgt -}
  | DisassembleFile FilePath {- src -}
  | ReassembleFile  FilePath {- src -} FilePath {- tgt -}
  deriving (Eq, Show)

runProblemFile_ :: (FilePath -> a)
                -> (FilePath -> a)
                -> (FilePath -> FilePath -> a)
                -> ProblemFile_
                -> a
runProblemFile_ a d r pf = case pf of
  AssembleFile        tgt  -> a tgt
  DisassembleFile src      -> d src
  ReassembleFile  src tgt  -> r src tgt

---

type Problem = (Int, ProblemType)

assembleProblem :: Int -> Problem
assembleProblem n = (n, Assemble)

disassembleProblem :: Int -> Problem
disassembleProblem n = (n, Disassemble)

reassembleProblem :: Int -> Problem
reassembleProblem n = (n, Reassemble)

runProblem :: (Int -> a)
           -> (Int -> a)
           -> (Int -> a)
           -> Problem
           -> a
runProblem a d r (i, t) = runProblemType (a i) (d i) (r i) t

---

data ProblemType
  = Assemble
  | Disassemble
  | Reassemble
  deriving (Eq, Show)

runProblemType :: a -> a -> a -> ProblemType -> a
runProblemType a d r t = case t of
  Assemble     -> a
  Disassemble  -> d
  Reassemble   -> r
