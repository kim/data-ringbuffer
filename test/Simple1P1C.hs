module Simple1P1C where

import Control.Concurrent
import Control.Monad
import Data.RingBuffer
import Data.IORef

import Debug.Trace

main = do
  consumer <- newConsumer (\x -> print $ "consumed: " ++ show x)
  barrier  <- newProducerBarrier [consumer]
  buf      <- newRingBuffer 10 0
  done     <- newEmptyMVar
  forkIO $ mapM (push' buf barrier) [1..20] >> return ()
  forkIO (consume' buf consumer >> putMVar done 1)
  -- takeMVar done >> print "done"
  print "k"
  where
    consume' b c@(Consumer _ cs) = do
      trace ("xxx") consume b [c]
      s <- readIORef cs
      if s > 10 then return () else consume' b c
    push' buf barr i = trace ("yyy:" ++ show i) push buf barr i
