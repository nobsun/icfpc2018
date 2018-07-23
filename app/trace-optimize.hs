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
import TraceEncoder
import TraceOptimizer
import State


data Options
  = Options
  { optSrcModel :: Maybe FilePath
  , optTgtModel :: Maybe FilePath
  , optOutputFile :: FilePath
  , optTrace :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> srcModelOption
  <*> tgtModelOption
  <*> outputOption
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

    outputOption :: Parser FilePath
    outputOption = strOption
      $  short 'o'
      <> metavar "TRACE"
      <> help "output trace file (.nbt)"

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
  let t2 = optimize m1 m2 t
  writeTraceFile (optOutputFile opt) t2

