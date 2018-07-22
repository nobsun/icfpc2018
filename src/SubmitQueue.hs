{-# OPTIONS -Wall #-}

module SubmitQueue where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import System.IO.Error (tryIOError)
import Data.Time
  (NominalDiffTime, diffUTCTime, getCurrentTime,
   FormatTime, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import System.FilePath ((</>))
import System.Directory (renameFile)
import System.Process (rawSystem, readProcess)
import System.Exit (ExitCode (..))

import Path (submitExport)


interval :: NominalDiffTime
interval = fromInteger $ 15 * 60

formatLogstamp :: FormatTime t => t -> String
formatLogstamp = formatTime defaultTimeLocale "%Y-%m-%d %HH:%MM:%SS"

putLog :: String -> IO ()
putLog s = do
  ts <- formatLogstamp <$> getCurrentTime
  putStrLn $ ts ++ ": " ++ s

submitLoop :: IO () -> IO ()
submitLoop submit_ =
    loop Nothing
  where
    loop maySubmitTs = do
      let delayRest prev = do
            current <- getCurrentTime
            let waitt = interval - diffUTCTime current prev
            putStrLn $ unwords
              [ formatLogstamp current ++ ":",
                "previouns post at ",
                formatLogstamp prev ++ ".",
                "so, wait", show waitt ++ "." ]
            threadDelay $ fromEnum waitt `quot` 1000000
      maybe (return ()) delayRest maySubmitTs
      putLog "now, run submit."
      putLog . either (\e -> "submit failed: " ++ show e) (\() -> "submit done.")
        =<< tryIOError submit_
      loop . Just =<< getCurrentTime

formatFilestamp :: FormatTime t => t -> String
formatFilestamp = formatTime defaultTimeLocale "%d-%H%M%D"

submit :: Chan String -> IO ()
submit c = do
  let queuedir = "/home/icfpc2018-queue"
  fn <- readChan c
  ts <- formatFilestamp <$> getCurrentTime
  let dfn = ts ++ "_" ++ fn
  renameFile (queuedir </> fn) (submitExport </> dfn)
  ioExitCode =<< rawSystem "./lib/apply-submit.sh" [dfn]


runExitCode :: a -> (Int -> a) -> ExitCode -> a
runExitCode s e ec = case ec of
  ExitSuccess   -> s
  ExitFailure c -> e c

ioExitCode :: ExitCode -> IO ()
ioExitCode = runExitCode (return ()) (fail . ("exited failure with code: " ++) . show)
