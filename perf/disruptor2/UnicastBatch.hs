module Main where

import           Control.Applicative  ((*>))
import           Control.Concurrent   ( newEmptyMVar
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
    done  <- newEmptyMVar
    start <- now

    let xs = chunk 10 [0..iterations]
    forkIO $ mapM_ (pub seqr vals) xs
    forkIO $ consumeAll (MVector vals) modmask (newBarrier seqr []) con done

    takeMVar done *> now >>= printTiming iterations start

    where
        bufferSize = 1024*8
        modmask    = bufferSize - 1

        idx n   = fromIntegral $ n .&. modmask
        {-# INLINE idx #-}
        chunk n = takeWhile (not . null) . map (take n) . iterate (drop n)

        pub sqr vs chnk = do
            let len = length chnk
                lst = chnk !! (len - 1)
            next <- claim sqr lst bufferSize
            mapM_ (upd vs) chnk
            publish sqr next len

        upd vs x = V.unsafeWrite vs (idx x) x
        {-# INLINE upd #-}

        consumeAll vec modm barr con lock = do
            consumeFrom vec modm barr con
            consumed <- consumerSeq con
            if consumed == iterations
                then putMVar lock ()
                else consumeAll vec modm barr con lock


-- vim: set ts=4 sw=4 et:

