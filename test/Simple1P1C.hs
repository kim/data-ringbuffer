module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.RingBuffer
import Data.IORef

import Debug.Trace

size :: Int
size = 1024*32

events :: [Int]
events = [0..30000]

main = do
  consumer <- newConsumer (\x -> print x)
  barrier  <- newProducerBarrier [consumer]
  buf      <- newRingBuffer size 0
  done     <- newEmptyMVar
  pdone    <- newEmptyMVar
  expseq   <- newIORef $ last events

  forkIO $ do
    mapM (push buf barrier) events
    c <- readIORef . cursor $ buf
    trace ("push done") putMVar pdone c
  forkIO $ consume' buf consumer expseq `finally` trace ("consumer stopped") putMVar done 1

  cur <- takeMVar pdone
  trace ("cursor @ " ++ show cur) writeIORef expseq cur
  takeMVar done >> print "done"
  where
    consume' b c@(Consumer _ cs) ex = do
      consume b [c]
      seq <- readIORef cs
      exp <- readIORef ex
      unless (seq >= exp) $ consume' b c ex
