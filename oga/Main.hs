module Main where

import System.Environment

import BinaryEncoder
import OgaBot
import Model

main :: IO ()
main = do
  (mdl:nbt:_) <- getArgs
  model <- readModel mdl
  let trs = getOgaBotTrace model
--  print trs
  mapM_ print trs
  writeTraceFile nbt trs
