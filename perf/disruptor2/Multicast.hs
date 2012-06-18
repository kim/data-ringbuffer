module Main where

import           Control.Concurrent   ( MVar
                                      , newEmptyMVar
                                      , putMVar
                                      , takeMVar
                                      , forkIO
                                      )
import           Control.DeepSeq      (rnf)
import           Control.Monad        (replicateM)
import           Data.Bits
import           Data.RingBuffer
import qualified Data.Vector.Mutable  as V
import Util


main :: IO ()
main = do
    cons  <- replicateM 3 $ newConsumer (return . rnf)
    seqr  <- newSequencer cons
    vals  <- V.replicate (fromIntegral bufferSize) 0
    dones <- replicateM 3 (newEmptyMVar :: IO (MVar ()))
    start <- now

    forkIO $ mapM_ (pub seqr vals . fromIntegral) [0..iterations]
    mapM_ (\(c,l) -> forkChild vals seqr c l) (zip cons dones)

    mapM_ takeMVar dones

    now >>= printTiming iterations start

    where
        bufferSize = 1024*8
        modmask    = bufferSize - 1

        idx n = fromIntegral $ n .&. modmask

        pub sqr vs i = do
            next <- claim sqr i bufferSize
            V.write vs (idx next) i
            publish sqr next 1

        forkChild vec seqr con lock = forkIO $
            consumeAll (MVector vec) modmask (newBarrier seqr []) con lock

        consumeAll vec modm barr con lock = do
            consumeFrom vec modm barr con
            consumed <- consumerSeq con
            if consumed == iterations
                then putMVar lock ()
                else consumeAll vec modm barr con lock
