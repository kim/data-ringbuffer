module Main where

import           Control.Applicative  ((*>))
import           Control.Concurrent   ( MVar
                                      , newEmptyMVar
                                      , putMVar
                                      , takeMVar
                                      , forkIO
                                      )
import           Control.DeepSeq      (rnf)
import           Data.Bits
import           Data.RingBuffer
import qualified Data.Vector.Mutable  as V
import Util


main :: IO ()
main = do
    con   <- newConsumer (return . rnf)
    seqr  <- newSequencer [con]
    vals  <- V.replicate (fromIntegral bufferSize) 0
    done  <- newEmptyMVar :: IO (MVar ())
    start <- now

    forkIO $ mapM_ (pub seqr vals . fromIntegral) [0..iterations]
    forkIO $ consumeAll (MVector vals) modmask (newBarrier seqr []) con done

    takeMVar done *> now >>= printTiming iterations start

    where
        bufferSize = 1024*8
        modmask    = bufferSize - 1

        idx n = fromIntegral $ n .&. modmask

        pub sqr vs i = do
            next <- claim sqr i bufferSize
            V.write vs (idx next) i
            publish sqr next

        consumeAll vec modm barr con lock = do
            consumeFrom vec modm barr con
            consumed <- consumerSeq con
            if consumed == iterations
                then putMVar lock ()
                else consumeAll vec modm barr con lock


-- vim: set ts=4 sw=4 et:
