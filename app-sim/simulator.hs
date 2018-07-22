{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import Options.Applicative
import System.Exit
import Text.Printf

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
  m@(Model _ mat) <- readModel $ optModel opt
  t <- readTraceFile $ optTrace opt
  let s = initialState (Just m) Nothing t
      s2 = execAll s
      success = mat == stMatrix s2
  printf "Assembly: %s\n" $ if success then "SUCCESS" else "FAILURE"
  printf "Energy: %d\n" $ stEnergy s2
  unless success $ exitFailure
