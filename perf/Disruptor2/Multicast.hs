module Disruptor2.Multicast (run) where

import Control.Concurrent     ( newEmptyMVar
                              , putMVar
                              , takeMVar
                              , forkIO
                              )
import Control.DeepSeq        (rnf)
import Control.Monad          (replicateM)
import Data.RingBuffer
import Data.RingBuffer.Vector
import Util


run :: Int -> IO ()
run i = do
    cons  <- replicateM 3 $ newConsumer (return . rnf)
    seqr  <- newSequencer cons
    buf   <- newRingBuffer bufferSize 0
    dones <- replicateM 3 newEmptyMVar
    start <- now

    forkIO $ mapM_ (pub buf seqr) [0..i]
    mapM_ (\(c,l) -> forkChild buf seqr c l) (zip cons dones)

    mapM_ takeMVar dones

    now >>= printTiming i start

    where
        bufferSize = 1024*8
        modmask    = bufferSize - 1

        pub buf seqr i' = publishTo buf modmask seqr i' i'

        forkChild buf seqr con lock = forkIO $
            consumeAll buf modmask (newBarrier seqr []) con lock

        consumeAll buf modm barr con lock = do
            consumeFrom buf modm barr con
            consumed <- consumerSeq con
            if consumed == i
                then putMVar lock ()
                else consumeAll buf modm barr con lock


-- vim: set ts=4 sw=4 et:
