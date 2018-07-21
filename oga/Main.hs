module Main where

import System.Environment

import BinaryEncoder
import OgaBot
import Model

main :: IO ()
main = do
  args <- getArgs
  mapM_ f args
  where
    f :: String -> IO ()
    f mdl = do
      model <- readModel mdl
      let trs = getOgaBotTrace model
--      mapM_ print trs -- for debug
      writeTraceFile (mkNbtName mdl) trs

mkNbtName :: String -> String
mkNbtName str =
  name ++ ".nbt"
  where
    name = takeWhile (/='_') str
