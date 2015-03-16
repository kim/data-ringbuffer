module Disruptor3.Unicast (run) where

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
    d    <- newSingleProducerRingBuffer (1024*8) (newIORef 0)
        >>= consumeWith (\ _ -> modifyIORef' xs (+1))
        >>= andThen     (readIORef >=> (\ x -> when (x >= i) $ atomically (putTMVar done ())))
        >>= start

    forM_ [0 .. i] $ publish d . flip writeIORef

    atomically $ takeTMVar done
    stop d

    nxs  <- readIORef xs
    when (nxs /= (i + 1)) $
        error $ "expected " ++ show (i + 1) ++ " consumed entries, got: " ++ show nxs

    now >>= printTiming i strt

-- vim: set ts=4 sw=4 et:
