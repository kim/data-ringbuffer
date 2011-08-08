{-# LANGUAGE BangPatterns #-}

module Data.RingBuffer
  ( RingBuffer
  , Consumer(..)
  , Entry
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

import Debug.Trace


data RingBuffer a = RingBuffer
  { cursor  :: {-# UNPACK #-} !(IORef Int)
  , entries :: ![Entry a]
  }

newtype Entry a = Entry { value :: IORef a }

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
  es <- mapM id $ replicate (ceilNextPowerOfTwo size) (newEntry e)
  return $ RingBuffer c es
{-# INLINE newRingBuffer #-}

newEntry :: a -> IO (Entry a)
newEntry v = do
  s <- newIORef v
  return $ Entry s
{-# INLINE newEntry #-}

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
      l = (length . entries $ b) - 1
      inc i = (i + 1) .&. l
  i <- atomicModifyIORef s (\old -> (inc old, inc old))
  ensureConsumersAreInRange i
  let val = value $ entries b !! i
      cur = cursor b
  writeIORef cur i
  trace ("push @" ++ show i) writeIORef val v
  where
    ensureConsumersAreInRange :: Int -> IO ()
    ensureConsumersAreInRange i = do
      xs <- mapM (readIORef . consumerSequence) (consumers p)
      let min  = minimum xs
          wrap = i - min
      trace ("min:" ++ show min ++ " wrap:" ++ show wrap) unless (wrap > min) $ yield >> ensureConsumersAreInRange i

consume :: RingBuffer a -> [Consumer a] -> IO ()
consume _ [] = return ()
consume buf (c:cs) = do
  let cseq = consumerSequence c
      es   = entries buf
      delta a b = if a == b then 1 else a - b
  nseq  <- readIORef cseq >>= \seq -> return $ seq + 1
  avail <- trace("waitFor " ++ show nseq) waitFor nseq
  batch <- mapM (readIORef . value) $ slice es nseq (delta avail nseq)
  writeIORef cseq avail
  mapM_ (consumeFn c) batch
  consume buf cs
  where
    waitFor :: Int -> IO Int
    waitFor seq = do
      avail <- readIORef . cursor $ buf
      if seq < avail then return avail else trace ("seq:" ++ show seq ++ " avail:" ++ show avail) yield >> waitFor seq

slice :: [a] -> Int -> Int -> [a]
slice xs i j = map snd
               $ filter (\(x,_) -> x >= i && x <= j)
               $ zip [1..] xs

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
