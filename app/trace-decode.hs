{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad
import Options.Applicative

import TraceDecoder


data Options
  = Options
  { optTrace :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> traceOption
  where
    traceOption :: Parser FilePath
    traceOption = argument str $ metavar "TRACE" <> help "trace file (.nbt)"

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser)
  $  fullDesc
  <> header "trace-decode"


main :: IO ()
main = do
  opt <- execParser parserInfo
  t <- readTraceFile $ optTrace opt
  forM_ t $ \cmd -> do
    print cmd
