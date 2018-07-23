{-# OPTIONS -Wall #-}

module Concurrent (
  concurrentIO,
  newLog,
  ) where

import Control.Monad (void, forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import System.IO (stdout, BufferMode (LineBuffering), hSetBuffering)


concurrentIO :: Int    -- ^ num of threads
             -> [IO a] -- ^ tasks
             -> IO ()
concurrentIO n as = do
  tq <- newChan
  wq <- newChan
  let thread = do
        mayT <- readChan tq
        case mayT of
          Nothing  ->  writeChan wq ()  -- end of thread
          Just t   ->  t *> thread      -- next task
  sequence_ . replicate n $ forkIO thread
  mapM_ (writeChan tq) $ map Just as ++ replicate n Nothing  -- enqueue tasks and end-marks
  sequence_ . replicate n $ readChan wq                      -- waiting finish

newLog :: IO (String -> IO ())
newLog = do
  hSetBuffering stdout LineBuffering
  c <- newChan
  void . forkIO . forever $ putStrLn =<< readChan c
  return $ writeChan c
