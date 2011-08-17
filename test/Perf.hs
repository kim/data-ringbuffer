{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.RingBuffer
import Data.IORef

import Debug.Trace

size :: Int
size = 1024*32

iterations :: Int
iterations = 3000000

main :: IO ()
main = do
  consumer <- newConsumer (return . rnf)
  barrier  <- newProducerBarrier [consumer]
  buf      <- newRingBuffer size 0
  done     <- newEmptyMVar

  forkIO $ produce buf barrier 0 >> print "producer done"
  forkIO $ consume' buf consumer `finally` putMVar done 1

  takeMVar done >> print "done"
  where
    produce b barr i =
      unless (i > iterations) $ push b barr i >> produce b barr (i + 1)

    consume' b c@(Consumer _ cs) = do
      consume b [c]
      i <- readIORef cs
      unless (i >= iterations) $ consume' b c
