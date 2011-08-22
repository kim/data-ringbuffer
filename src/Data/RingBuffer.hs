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

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.Int
import Data.IORef

import qualified Data.Vector as VG
import qualified Data.Vector.Mutable as V

-- import Debug.Trace


data RingBuffer a = RingBuffer {-# UNPACK #-} !(IORef Int64)   -- ^ cursor
                               {-# UNPACK #-} !(V.IOVector a)  -- ^ entries
                                              Int64            -- ^ size
                                              Int64            -- ^ ring mod mask (size - 1)

data Consumer a = Consumer (a -> IO ())    -- ^ consuming function
            {-# UNPACK #-} !(IORef Int64)  -- ^ consumer sequence

data ProducerBarrier a = ProducerBarrier [Consumer a]    -- ^ consumers to track
                          {-# UNPACK #-} !(IORef Int64)  -- ^ producer sequence


newRingBuffer :: Int -> a -> IO (RingBuffer a)
newRingBuffer size e = do
  c  <- newIORef (-1)
  es <- V.replicate (ceilNextPowerOfTwo size) e
  let s = fromIntegral . V.length $ es
      m = s - 1
  return $ RingBuffer c es s m
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
push (RingBuffer cursor entries l m)
     (ProducerBarrier consumers pseq) v = do
  i <- incrementAndGet pseq
  {-# SCC "inrange" #-} consumersInRange i
  V.unsafeWrite entries (rmod i m) v
  writeIORef cursor i
  where
    consumersInRange i = minConsumerSeq >>= \mins ->
                         unless (i - l <= mins) $
                           yield >> consumersInRange i

    minConsumerSeq     = fromIntegral . minimum <$>
                           mapM (\(Consumer _ s) -> readIORef s) consumers

consume :: RingBuffer a -> [Consumer a] -> IO ()
consume _ [] = return ()
consume b@(RingBuffer cursor entries _ m) (Consumer f cseq : cs) = do
  cseq' <- readIORef cseq
  avail <- {-# SCC "waitfor" #-} waitFor cseq'
  _     <- {-# SCC "mapconsume" #-} VG.mapM_ f <$>
           {-# SCC "slice" #-} unsafeSlice entries m cseq' avail
  writeIORef cseq avail
  consume b cs
  where
    waitFor i = readIORef cursor >>= \avail ->
                if i < avail then return avail else yield >> waitFor i


-- | Utilities

ceilNextPowerOfTwo :: Int -> Int
ceilNextPowerOfTwo i = shiftL 1 (32 - numberOfLeadingZeros (i - 1))
{-# INLINE ceilNextPowerOfTwo #-}

numberOfLeadingZeros :: Int -> Int
numberOfLeadingZeros i = nolz i 1
  where
    nolz 0 _ = 32
    nolz i' n | shiftR i' 16 == 0 = nolz (shiftL i' 16) (n + 16)
              | shiftR i' 24 == 0 = nolz (shiftL i'  8)  (n +  8)
              | shiftR i' 28 == 0 = nolz (shiftL i'  4)  (n +  4)
              | shiftR i' 30 == 0 = nolz (shiftL i'  2)  (n +  2)
              | otherwise = n - shiftR i' 31
{-# INLINE numberOfLeadingZeros #-}

rmod :: Int64 -> Int64 -> Int
rmod a b = {-# SCC "rmod" #-} fromIntegral $ a .&. b
{-# INLINE rmod #-}

incrementAndGet :: IORef Int64 -> IO Int64
incrementAndGet i = atomicModifyIORef i (\x -> (x + 1, x + 1))
{-# INLINE incrementAndGet #-}

unsafeSlice :: V.IOVector a -> Int64 -> Int64 -> Int64 -> IO (VG.Vector a)
unsafeSlice v m a b = do
  let start = rmod (a + 1) m
      len   = (rmod b m) - start
  VG.unsafeFreeze $ V.unsafeSlice start len v
{-# INLINE unsafeSlice #-}
