module Disruptor3.Diamond (run) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import Data.RingBuffer
import Util


data FizzBuzz = FizzBuzz
    { fizz :: IORef Bool
    , buzz :: IORef Bool
    , pub  :: IORef Int
    }

run :: Int -> IO ()
run i = do
    strt  <- now
    done  <- atomically newEmptyTMVar
    count <- newIORef (0 :: Int)
    d     <- newSingleProducerRingBuffer (1024*8) (FizzBuzz <$> newIORef False <*> newIORef False <*> newIORef 0)
         >>= consumeWith (\ (FizzBuzz f _ _) -> writeIORef f True)
         >>= andAlso     (\ (FizzBuzz _ b _) -> writeIORef b True)
         >>= andThen     (\ (FizzBuzz f b _) -> do
                            f' <- readIORef f
                            b' <- readIORef b
                            when (f' && b') $
                                atomicModifyIORef' count (\ x -> (x+1,())))
         >>= andThen     (readIORef . pub >=> (\ x -> when (x >= i) $ atomically (putTMVar done ())))
         >>= start

    forM_ [0 .. i] $ \ i' ->
        publish d (flip writeIORef i' . pub)

    atomically $ takeTMVar done
    stop d

    fizzbuzzes <- readIORef count
    when (fizzbuzzes /= i + 1) $
        error $ "expected " ++ show (i + 1) ++ " consumed entries, got: " ++ show fizzbuzzes

    now >>= printTiming i strt

-- vim: set ts=4 sw=4 et:e
