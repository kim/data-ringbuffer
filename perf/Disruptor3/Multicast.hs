module Disruptor3.Multicast (run) where

import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import Data.RingBuffer
import Util


run :: Int -> IO ()
run i = do
    strt <- now
    done <- atomically newEmptyTMVar
    xs   <- newIORef (0 :: Int)
    d    <- newRingBuffer (1024*8) (newIORef 0)
        >>= consumeWith (\ _ -> atomicModifyIORef' xs (\ x -> (x+1,())))
        >>= andAlso     (\ _ -> atomicModifyIORef' xs (\ x -> (x+1,())))
        >>= andAlso     (\ _ -> atomicModifyIORef' xs (\ x -> (x+1,())))
        >>= andThen     (readIORef >=> (\ x -> when (x >= i) $ atomically (putTMVar done ())))
        >>= start

    forM_ [0 .. i] $ publish d . flip writeIORef

    atomically $ takeTMVar done
    stop d

    nxs  <- readIORef xs
    when (nxs /= (i + 1) * 3) $
        error $ "expected " ++ show (i + 1) ++ " consumed entries, got: " ++ show nxs

    now >>= printTiming i strt

-- vim: set ts=4 sw=4 et:e
