module Main where

import           Control.Concurrent     ( newEmptyMVar
                                        , putMVar
                                        , takeMVar
                                        , forkIO
                                        )
import           Control.DeepSeq        (rnf)
import           Control.Monad          (replicateM)
import           Data.RingBuffer
import           Data.RingBuffer.Vector
import           Util


main :: IO ()
main = do
    cons  <- replicateM 3 $ newConsumer (return . rnf)
    seqr  <- newSequencer cons
    buf   <- newRingBuffer bufferSize 0
    dones <- replicateM 3 newEmptyMVar
    start <- now

    forkIO $ mapM_ (pub buf seqr) [0..iterations]
    mapM_ (\(c,l) -> forkChild buf seqr c l) (zip cons dones)

    mapM_ takeMVar dones

    now >>= printTiming iterations start

    where
        bufferSize = 1024*8
        modmask    = bufferSize - 1

        pub buf seqr i = publishTo buf modmask seqr i i

        forkChild buf seqr con lock = forkIO $
            consumeAll buf modmask (newBarrier seqr []) con lock

        consumeAll buf modm barr con lock = do
            consumeFrom buf modm barr con
            consumed <- consumerSeq con
            if consumed == iterations
                then putMVar lock ()
                else consumeAll buf modm barr con lock
