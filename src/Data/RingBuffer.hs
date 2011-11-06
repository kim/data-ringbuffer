{-# LANGUAGE BangPatterns #-}

module Data.RingBuffer
  ( RingBuffer
  , Consumer
  , ProducerBarrier
  , newRingBuffer
  , newConsumer
  , newProducerBarrier
  , push
  , pushAll
  , consume
  , consumerSequence
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.Int
import Data.IORef

import qualified Data.Vector as VG
import qualified Data.Vector.Mutable as V


data RingBuffer a = RingBuffer
     {-# UNPACK #-} !(IORef Int64)   -- ^ cursor
     {-# UNPACK #-} !(V.IOVector a)  -- ^ entries
                    Int64            -- ^ size
                    Int64            -- ^ ring mod mask (size - 1)

data Consumer a = Consumer
                  (a -> IO ())    -- ^ consuming function
   {-# UNPACK #-} !(IORef Int64)  -- ^ consumer sequence

data ProducerBarrier a = ProducerBarrier
                         [Consumer a]    -- ^ consumers to track
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
push b pb v = claim b pb 1 >>= \i -> unsafePush b i v

pushAll :: RingBuffer a -> ProducerBarrier a -> [a] -> IO ()
pushAll b pb vs = claim b pb l >>= \i -> mapM_ unsafePush' $ s i
  where
    l = fromIntegral . length $ vs
    s i = zip vs [(i-l)..i]
    unsafePush' (v, i) = unsafePush b i v

consume :: RingBuffer a -> Consumer a -> IO ()
consume b@(RingBuffer _ entries _ m) (Consumer f cseq) = do
  cseq' <- readIORef cseq
  avail <- waitFor b cseq'
  _     <- VG.mapM_ f <$> unsafeSlice entries m cseq' avail
  writeIORef cseq avail

consumerSequence :: Consumer a -> IO Int64
consumerSequence (Consumer _ s) = readIORef s
{-# INLINE consumerSequence #-}

-- | Utilities

unsafePush :: RingBuffer a -> Int64 -> a -> IO ()
unsafePush (RingBuffer cursor entries _ m) i v = do
  V.unsafeWrite entries (rmod i m) v
  writeIORef cursor i
{-# INLINE unsafePush #-}

claim :: RingBuffer a -> ProducerBarrier a -> Int64 -> IO Int64
claim (RingBuffer _ _ l _) (ProducerBarrier consumers pseq) s = do
  i <- incrementAndGet pseq s
  ensureConsumersAreInRange consumers l i
  return i
{-# INLINE claim #-}

ensureConsumersAreInRange :: [Consumer a] -> Int64 -> Int64 -> IO ()
ensureConsumersAreInRange consumers l i = do
  mins <- minConsumerSeq consumers
  unless (i - l <= mins) $ yield >> ensureConsumersAreInRange consumers l i
{-# INLINE ensureConsumersAreInRange #-}

minConsumerSeq :: [Consumer a] -> IO Int64
minConsumerSeq cs = fromIntegral . minimum <$>
                    mapM (\(Consumer _ s) -> readIORef s) cs
{-# INLINE minConsumerSeq #-}

waitFor :: RingBuffer a -> Int64 -> IO Int64
waitFor b@(RingBuffer cursor _ _ _) i = do
  cursor' <- readIORef cursor
  if i < cursor' then return cursor' else yield >> waitFor b i
{-# INLINE waitFor #-}

ceilNextPowerOfTwo :: Int -> Int
ceilNextPowerOfTwo i = shiftL 1 (32 - numberOfLeadingZeros (i - 1))
{-# INLINE ceilNextPowerOfTwo #-}

numberOfLeadingZeros :: Int -> Int
numberOfLeadingZeros i = nolz i 1
  where
    nolz 0 _ = 32
    nolz i' n | shiftR i' 16 == 0 = nolz (shiftL i' 16) (n + 16)
              | shiftR i' 24 == 0 = nolz (shiftL i'  8) (n +  8)
              | shiftR i' 28 == 0 = nolz (shiftL i'  4) (n +  4)
              | shiftR i' 30 == 0 = nolz (shiftL i'  2) (n +  2)
              | otherwise = n - shiftR i' 31
{-# INLINE numberOfLeadingZeros #-}

rmod :: Int64 -> Int64 -> Int
rmod a b = {-# SCC "rmod" #-} fromIntegral $ a .&. b
{-# INLINE rmod #-}

incrementAndGet :: IORef Int64 -> Int64 -> IO Int64
incrementAndGet ioref delta = atomicModifyIORef ioref $ pair . (+delta)
  where
    pair x = (x, x)
{-# INLINE incrementAndGet #-}

unsafeSlice :: V.IOVector a -> Int64 -> Int64 -> Int64 -> IO (VG.Vector a)
unsafeSlice v m a b = do
  let start = rmod (a + 1) m
      len   = rmod b m - start
  VG.unsafeFreeze $ V.unsafeSlice start len v
{-# INLINE unsafeSlice #-}
