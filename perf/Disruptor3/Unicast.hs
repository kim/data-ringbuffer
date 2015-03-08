module Disruptor3.Unicast (run) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import Data.RingBuffer
import Util


run :: Int -> IO ()
run i = do
    strt <- now
    done <- atomically newEmptyTMVar
    xs   <- newIORef ([] :: [Int])
    d    <- newSingleProducerRingBuffer (1024*8) (newIORef 0)
        >>= consumeWith (readIORef >=> (\ x -> modifyIORef' xs (x:)))
        >>= andThen     (readIORef >=> (\ x -> when (x >= i) $ atomically (putTMVar done ())))
        >>= start

    forM_ [0 .. i] $ publish d . flip writeIORef

    atomically $ takeTMVar done
    stop d

    nxs  <- length <$> readIORef xs
    when (nxs /= (i + 1)) $
        error $ "expected " ++ show (i + 1) ++ " consumed entries, got: " ++ show nxs

    now >>= printTiming i strt

-- vim: set ts=4 sw=4 et:
