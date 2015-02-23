module Disruptor2.UnicastBatch (run) where

import Control.Applicative    ((*>))
import Control.Concurrent     ( newEmptyMVar
                              , putMVar
                              , takeMVar
                              , forkIO
                              )
import Control.DeepSeq        (rnf)
import Data.RingBuffer
import Data.RingBuffer.Vector
import Util

run :: Int -> IO ()
run i = do
    con   <- newConsumer (return . rnf)
    seqr  <- newSequencer [con]
    buf   <- newRingBuffer bufferSize 0
    done  <- newEmptyMVar
    start <- now

    let xs = chunk 10 [0..i]
    forkIO $ mapM_ (pub buf seqr) xs
    forkIO $ consumeAll buf modmask (newBarrier seqr []) con done

    takeMVar done *> now >>= printTiming i start

    where
        bufferSize = 1024*8
        modmask    = bufferSize - 1

        chunk n = takeWhile (not . null) . map (take n) . iterate (drop n)

        pub buf sqr chnk = do
            let len = length chnk
                lst = chnk !! (len - 1)
            batchPublishTo buf modmask sqr lst chnk

        consumeAll buf modm barr con lock = do
            consumeFrom buf modm barr con
            consumed <- consumerSeq con
            if consumed == i
                then putMVar lock ()
                else consumeAll buf modm barr con lock


-- vim: set ts=4 sw=4 et:

