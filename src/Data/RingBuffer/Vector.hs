module Data.RingBuffer.Vector
    ( MVector(..)
    , newRingBuffer
    , consumeFrom
    , publishTo
    , batchPublishTo
    , concPublishTo
    , concBatchPublishTo
    )
where

import           Control.Monad                     (unless)
import           Data.Bits
import           Data.Vector.Generic.Mutable       (mstream)
import qualified Data.Vector.Mutable               as MV
import qualified Data.Vector.Fusion.Stream.Monadic as S

import           Data.RingBuffer
import           Data.RingBuffer.Internal
import           Data.RingBuffer.Types


newtype MVector a = MVector (MV.IOVector a)


newRingBuffer :: Int -> a -> IO (MVector a)
newRingBuffer size zero = do
    mvec <- MV.replicate (ceilNextPowerOfTwo size) zero
    return (MVector mvec)

    where
        ceilNextPowerOfTwo i = shiftL 1 (32 - numberOfLeadingZeros (i - 1))

        numberOfLeadingZeros i = nlz i 1
            where
                nlz 0 _ = 32
                nlz i' n | shiftR i' 16 == 0 = nlz (shiftL i' 16) (n + 16)
                         | shiftR i' 24 == 0 = nlz (shiftL i'  8) (n +  8)
                         | shiftR i' 28 == 0 = nlz (shiftL i'  4) (n +  4)
                         | shiftR i' 30 == 0 = nlz (shiftL i'  2) (n +  2)
                         | otherwise = n - shiftR i' 31
{-# INLINE newRingBuffer #-}

consumeFrom :: MVector a -> Int -> Barrier -> Consumer a -> IO ()
consumeFrom (MVector mvec) modm barr (Consumer fn sq) = do
    next  <- addAndGet sq 1
    avail <- waitFor barr next

    let start = next .&. modm
        len   = avail - next
        (_,t) = MV.splitAt start mvec
        tlen  = MV.length t

    S.mapM_ fn . mstream . MV.take len $ t
    unless (tlen >= len) $
        S.mapM_ fn . mstream . MV.take (len - tlen) $ mvec

    writeSeq sq avail
{-# INLINE consumeFrom #-}

publishTo :: MVector a -> Int -> Sequencer -> Int -> a -> IO ()
publishTo (MVector mvec) modm seqr i v = do
    next <- claim seqr i (MV.length mvec)
    MV.unsafeWrite mvec (next .&. modm) v
    publish seqr next 1
{-# INLINE publishTo #-}

batchPublishTo :: MVector a -> Int -> Sequencer -> Int -> [a] -> IO ()
batchPublishTo (MVector mvec) modm seqr i vs = do
    next <- claim seqr i (MV.length mvec)
    mapM_ update $ zip [next - len + 1..next] vs
    publish seqr next len

    where
        len = length vs

        update (n,x) = MV.unsafeWrite mvec (n .&. modm) x
{-# INLINE batchPublishTo #-}

concPublishTo :: MVector a -> Int -> Sequencer -> Sequence -> a -> IO ()
concPublishTo (MVector mvec) modm seqr sq v = do
    next <- nextSeq seqr sq (MV.length mvec)
    MV.unsafeWrite mvec (next .&. modm) v
    publish seqr next 1
{-# INLINE concPublishTo #-}

concBatchPublishTo :: MVector a -> Int -> Sequencer -> Sequence -> Int -> [a] -> IO ()
concBatchPublishTo (MVector mvec) modm seqr sq i vs = do
    next <- nextBatch seqr sq i (MV.length mvec)
    mapM_ update $ zip [next - len + 1..next] vs
    publish seqr next len

    where
        len = length vs

        update (n,x) = MV.unsafeWrite mvec (n .&. modm) x
{-# INLINE concBatchPublishTo #-}


-- vim: set ts=4 sw=4 et:
