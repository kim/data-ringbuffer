module Main where

import           Control.Applicative    ((*>))
import           Control.Concurrent     ( newEmptyMVar
                                        , putMVar
                                        , takeMVar
                                        , forkIO
                                        )
import           Control.DeepSeq        (rnf)
import           Data.RingBuffer
import           Data.RingBuffer.Vector
import           Util


main :: IO ()
main = do
    con   <- newConsumer (return . rnf)
    seqr  <- newSequencer [con]
    buf   <- newRingBuffer bufferSize 0
    done  <- newEmptyMVar
    start <- now

    forkIO $ mapM_ (pub buf seqr) [0..iterations]
    forkIO $ consumeAll buf modmask (newBarrier seqr []) con done

    takeMVar done *> now >>= printTiming iterations start

    where
        bufferSize = 1024*8
        modmask    = bufferSize - 1

        pub buf seqr i = publishTo buf modmask seqr i i

        consumeAll buf modm barr con lock = do
            consumeFrom buf modm barr con
            consumed <- consumerSeq con
            if consumed == iterations
                then putMVar lock ()
                else consumeAll buf modm barr con lock


-- vim: set ts=4 sw=4 et:
