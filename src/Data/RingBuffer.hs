{-# LANGUAGE BangPatterns #-}

module Data.RingBuffer
  ( RingBuffer
  , Consumer
  , Entry
  , ProducerBarrier
  , newRingBuffer
  , newConsumer
  , newEntry
  , newProducerBarrier
  , push
  , consume
  ) where

import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.IORef

import Prelude hiding (seq)

data RingBuffer a = RingBuffer
  { cursor  :: {-# UNPACK #-} !(IORef Int)
  , entries :: ![Entry a]
  }

data Entry a = Entry { value :: {-# UNPACK #-} !(IORef a) }

data Consumer a = Consumer
  { consumeFn :: a -> IO ()
  , consumerSequence :: {-# UNPACK #-} !(IORef Int)
  }

data ProducerBarrier a = ProducerBarrier
  { consumers :: [Consumer a]
  , producerSequence :: {-# UNPACK #-} !(IORef Int)
  }


newRingBuffer :: [Entry a] -> IO (RingBuffer a)
newRingBuffer xs = do
  c <- newIORef 0
  return $ RingBuffer c xs
{-# INLINE newRingBuffer #-}

newEntry :: a -> IO (Entry a)
newEntry v = do
  s <- newIORef v
  return $ Entry s
{-# INLINE newEntry #-}

newConsumer :: (a -> IO ()) -> IO (Consumer a)
newConsumer fn = do
  s <- newIORef 0
  return $ Consumer fn s
{-# INLINE newConsumer #-}

newProducerBarrier :: [Consumer a] -> IO (ProducerBarrier a)
newProducerBarrier cs = do
  s <- newIORef 0
  return $ ProducerBarrier cs s
{-# INLINE newProducerBarrier #-}

push :: RingBuffer a -> ProducerBarrier a -> a -> IO ()
push b p v = do
  let s = producerSequence p
      l = length . entries $ b
      inc i = i + 1 .&. l
  i <- atomicModifyIORef s (\old -> do let new = inc old
                                       (new, new))
  ensureConsumersAreInRange p i
  let val = value $ entries b !! i
      cur = cursor b
  writeIORef cur i
  writeIORef val v

consume :: RingBuffer a -> [Consumer a] -> IO ()
consume _ [] = return ()
consume b (c:cs) = do
  let cseq = consumerSequence c
      es   = entries b
  nseq  <- readIORef cseq >>= \seq -> return $ seq + 1
  avail <- waitFor b nseq
  batch <- mapM (readIORef . value) $ slice es nseq (avail - nseq)
  mapM_ (consumeFn c) batch
  writeIORef cseq avail
  consume b cs

waitFor :: RingBuffer a -> Int -> IO Int
waitFor b seq = do
  avail <- readIORef . cursor $ b
  if avail < seq then yield >> waitFor b seq else return avail

ensureConsumersAreInRange :: ProducerBarrier a -> Int -> IO ()
ensureConsumersAreInRange p i = do
  xs <- mapM (readIORef . consumerSequence) (consumers p)
  unless (i <= minimum xs) $ ensureConsumersAreInRange p i

slice :: [a] -> Int -> Int -> [a]
slice xs i j = map snd
               $ filter (\(x,_) -> x >= i && x <= j)
               $ zip [1..] xs
