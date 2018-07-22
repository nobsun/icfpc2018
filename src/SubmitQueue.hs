{-# OPTIONS -Wall #-}

module SubmitQueue (
  submitGateway,
  ) where

import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import System.IO.Error (tryIOError)
import Data.List (isSuffixOf)
import Data.Time
  (NominalDiffTime, diffUTCTime, getCurrentTime,
   FormatTime, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import System.IO (stdout, BufferMode (LineBuffering), hSetBuffering)
import System.FilePath ((</>), (<.>))
import System.Directory (renameFile)
import System.Process (rawSystem, readProcess)
import System.Exit (ExitCode (..))

import Path (submitExport)


submitGateway :: IO ()
submitGateway = do
  hSetBuffering stdout LineBuffering
  putLog <- do
    put <- newLog
    return $ \s -> do
      ts <- formatLogStamp <$> getCurrentTime
      put $ ts ++ ": " ++ s
  queue  <- newChan
  void . forkIO . submitLoop putLog $ submit queue
  waitRequest putLog queue

interval :: NominalDiffTime
interval = fromInteger $ 15 * 60

newLog :: IO (String -> IO ())
newLog = do
  c <- newChan
  void . forkIO . forever $ putStrLn =<< readChan c
  return $ writeChan c

formatLogStamp :: FormatTime t => t -> String
formatLogStamp = formatTime defaultTimeLocale "%Y-%m-%d %HH:%MM:%SS"

submitLoop :: (String -> IO ()) -> IO () -> IO ()
submitLoop putLog_ submit_ =
    loop Nothing
  where
    putLog = putLog_ . ("submit: " ++)
    loop maySubmitTs = do
      let delayRest prev = do
            current <- getCurrentTime
            let waitt = interval - diffUTCTime current prev
            putLog $ unwords
              [ "previouns post at ",
                formatLogStamp prev ++ ".",
                "so, wait", show waitt ++ "." ]
            threadDelay $ fromEnum waitt `quot` 1000000
      maybe (return ()) delayRest maySubmitTs
      putLog "now, run submit."
      putLog . either (\e -> "submit failed: " ++ show e) (\() -> "submit done.")
        =<< tryIOError submit_
      loop . Just =<< getCurrentTime

formatFileStamp :: FormatTime t => t -> String
formatFileStamp = formatTime defaultTimeLocale "%d-%H%M%D"

queuedir :: FilePath
queuedir = "/home/icfpc2018-queue"

submit :: Chan FilePath -> IO ()
submit q = do
  fn <- readChan q
  ts <- formatFileStamp <$> getCurrentTime
  let dfn = ts ++ "_" ++ fn
  renameFile (queuedir </> fn) (submitExport </> dfn)
  ioExitCode =<< rawSystem "./lib/apply-submit.sh" [dfn]

waitRequest :: (String -> IO ()) -> Chan FilePath -> IO ()
waitRequest putLog_ q = do
  let postdir = "/home/icfpc2018-post/"
      putLog = putLog_ . ("request: " ++)
      process req = case req of
        [_d, _ev, fn] | ".zip" `isSuffixOf` fn -> do
          ts <- formatFileStamp <$> getCurrentTime
          let qfn = ts <.> "zip"
          putLog $ "enqueue: " ++ fn ++ " --> " ++ qfn
          renameFile (postdir </> fn) (queuedir </> qfn)
          writeChan q qfn
        [_d, _ev, fn] ->
          putLog $ "not zip, ignored: " ++ fn
        _             -> return ()
  reqs <- map words . lines
          <$> readProcess "/usr/bin/inotifywait" ["-m", "-e", "close_write", postdir] ""
  mapM_ process reqs

runExitCode :: a -> (Int -> a) -> ExitCode -> a
runExitCode s e ec = case ec of
  ExitSuccess   -> s
  ExitFailure c -> e c

ioExitCode :: ExitCode -> IO ()
ioExitCode = runExitCode (return ()) (fail . ("exited failure with code: " ++) . show)
