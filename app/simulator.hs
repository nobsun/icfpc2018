{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import Options.Applicative
import System.Exit
import Text.Printf

import Matrix
import Model
import TraceDecoder
import State
import Sim


data Options
  = Options
  { optModel :: FilePath
  , optTrace :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> modelOption
  <*> traceOption
  where
    modelOption :: Parser FilePath
    modelOption = argument str $ metavar "MODEL" <> help "model file (.mdl)"

    traceOption :: Parser FilePath
    traceOption = argument str $ metavar "TRACE" <> help "trace file (.nbt)"

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser)
  $  fullDesc
  <> header "simulator"


main :: IO ()
main = do
  opt <- execParser parserInfo
  m@(Model res mat) <- readModel $ optModel opt
  t <- readTraceFile $ optTrace opt
  let s = initialStateForAssembly m t
      s2 = execAll s
      success = stTgtMatrix s == stMatrix s2
  putStrLn $ if success then "Success::" else "Failure::"
  printf "Time: %d\n" $ stTime s2
  printf "Commands: %d\n" $ stCommands s2
  printf "Energy: %d\n" $ stEnergy s2
  unless success $ exitFailure
