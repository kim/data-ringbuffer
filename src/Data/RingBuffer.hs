{-# LANGUAGE BangPatterns #-}

module Data.RingBuffer
  ( RingBuffer(..)
  , Consumer(..)
  , ProducerBarrier
  , newRingBuffer
  , newConsumer
  , newProducerBarrier
  , push
  , consume
  ) where

import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.IORef

import Prelude hiding (seq, min)

import qualified Data.Vector.Mutable as V

import Debug.Trace


data RingBuffer a = RingBuffer
  { cursor  :: {-# UNPACK #-} !(IORef Int)
  , entries :: {-# UNPACK #-} !(V.IOVector a)
  }

data Consumer a = Consumer
  { consumeFn :: a -> IO ()
  , consumerSequence :: {-# UNPACK #-} !(IORef Int)
  }

data ProducerBarrier a = ProducerBarrier
  { consumers :: [Consumer a]
  , producerSequence :: {-# UNPACK #-} !(IORef Int)
  }


newRingBuffer :: Int -> a -> IO (RingBuffer a)
newRingBuffer size e = do
  c  <- newIORef (-1)
  es <- V.replicate (ceilNextPowerOfTwo size) e
  return $ RingBuffer c es
{-# INLINE newRingBuffer #-}

newConsumer :: (a -> IO ()) -> IO (Consumer a)
newConsumer fn = do
  s <- newIORef (-1)
  return $ Consumer fn s
{-# INLINE newConsumer #-}

newProducerBarrier :: [Consumer a] -> IO (ProducerBarrier a)
newProducerBarrier cs = do
  s <- newIORef (-1)
  return $ ProducerBarrier cs s
{-# INLINE newProducerBarrier #-}

push :: RingBuffer a -> ProducerBarrier a -> a -> IO ()
push b p v = do
  let s = producerSequence p
  i <- atomicModifyIORef s (\old -> (old + 1, old + 1))
  {-# SCC "in_range" #-} ensureConsumersAreInRange i
  V.write (entries b) (i .&. ringModMask b) v
  writeIORef (cursor b) i
  where
    ensureConsumersAreInRange i = do
      xs <- mapM (readIORef . consumerSequence) (consumers p)
      let min  = minimum xs
          wrap = i - (V.length . entries $ b)
      unless (wrap <= min) $ yield >> ensureConsumersAreInRange i

consume :: RingBuffer a -> [Consumer a] -> IO ()
consume _ [] = return ()
consume buf (c:cs) = do
  let cseq = consumerSequence c
      es   = entries buf
      m    = ringModMask buf
  cseq' <- readIORef cseq
  avail <- {-# SCC "wait_for" #-} waitFor cseq'
  batch <- {-# SCC "slice_batch" #-} mapM (\x -> V.read es (x .&. m)) [(cseq' + 1)..avail]
  mapM_ (consumeFn c) batch
  writeIORef cseq avail
  consume buf cs
  where
    waitFor :: Int -> IO Int
    waitFor seq = do
      avail <- readIORef . cursor $ buf
      if seq < avail
        then return avail
        else yield >> waitFor seq

ceilNextPowerOfTwo :: Int -> Int
ceilNextPowerOfTwo i = shiftL 1 (32 - numberOfLeadingZeros (i - 1))

numberOfLeadingZeros :: Int -> Int
numberOfLeadingZeros i = nolz i 1
  where
    nolz 0 _ = 32
    nolz i' n | shiftR i' 16 == 0 = nolz (shiftL i' 16) (n + 16)
              | shiftR i' 24 == 0 = nolz (shiftL i' 8)  (n +  8)
              | shiftR i' 28 == 0 = nolz (shiftL i' 4)  (n +  4)
              | shiftR i' 30 == 0 = nolz (shiftL i' 2)  (n +  2)
              | otherwise = n - (shiftR i' 31)

ringModMask :: RingBuffer a -> Int
ringModMask b = (V.length . entries $ b) - 1
