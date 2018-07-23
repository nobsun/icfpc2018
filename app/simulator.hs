{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import Data.Maybe
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
  { optSrcModel :: Maybe FilePath
  , optTgtModel :: Maybe FilePath
  , optTrace :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> srcModelOption
  <*> tgtModelOption
  <*> traceOption
  where
    srcModelOption :: Parser (Maybe FilePath)
    srcModelOption = optional $ strOption
      $  long "src"
      <> metavar "MODEL"
      <> help "source model file (.mdl)"

    tgtModelOption :: Parser (Maybe FilePath)
    tgtModelOption = optional $ strOption
      $  long "tgt"
      <> metavar "MODEL"
      <> help "target model file (.mdl)"

    traceOption :: Parser FilePath
    traceOption = argument str $ metavar "TRACE" <> help "trace file (.nbt)"

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser)
  $  fullDesc
  <> header "simulator"


main :: IO ()
main = do
  opt <- execParser parserInfo

  m1' <- forM (optSrcModel opt) readModel
  m2' <- forM (optTgtModel opt) readModel
  let res = fromJust $ fmap mdResolution m1' `mplus` fmap mdResolution m2'
  let m1 = fromMaybe (emptyModel res) m1'
      m2 = fromMaybe (emptyModel res) m2'

  t <- readTraceFile $ optTrace opt

  let s = initialStateForReassembly m1 m2 t
      s2 = execAll s
      success = stTgtMatrix s == stMatrix s2

  putStrLn $ if success then "Success::" else "Failure::"
  printf "Time: %d\n" $ stTime s2
  printf "Commands: %d\n" $ stCommands s2
  printf "Energy: %d\n" $ stEnergy s2
  unless success $ exitFailure
