module Disruptor2.Unicast (run) where

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

    forkIO $ mapM_ (pub buf seqr) [0..i]
    forkIO $ consumeAll buf (newBarrier seqr []) con done

    takeMVar done *> now >>= printTiming i start

  where
    bufferSize = 1024*8

    pub buf seqr i' = publishTo buf seqr i' i'

    consumeAll buf barr con lock = do
        consumeFrom buf barr con
        consumed <- consumerSeq con
        if consumed == i
            then putMVar lock ()
            else consumeAll buf barr con lock


-- vim: set ts=4 sw=4 et:
