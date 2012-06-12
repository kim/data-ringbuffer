module Data.RingBuffer
    ( -- * Types
      Barrier
    , Consumer
    , Sequence
    , Sequencer

    -- * Classes
    , Buffer(..)
    , MVector(..)

    -- * Value Constructors
    , newSequencer
    , newConsumer
    , newBarrier

    -- * Concurrent Access, aka Disruptor API
    , claim
    , nextSeq
    , waitFor
    , publish

    -- * Util
    , consumerSeq
    )
where

import           Control.Applicative  ((<$>), (*>))
import           Control.Concurrent   (yield)
import           Control.Monad        (unless)
import           Data.Bits
import           Data.CAS
import           Data.Int
import           Data.IORef
import qualified Data.Vector          as V
import qualified Data.Vector.Mutable  as MV


data Sequence   = Sequence {-# UNPACK #-} !Int64
                           {-# UNPACK #-} !Int64
                           {-# UNPACK #-} !Int64
                           {-# UNPACK #-} !Int64
                           {-# UNPACK #-} !Int64
                           {-# UNPACK #-} !Int64
                           {-# UNPACK #-} !Int64
                           !(IORef Int64)
                           {-# UNPACK #-} !Int64
                           {-# UNPACK #-} !Int64
                           {-# UNPACK #-} !Int64
                           {-# UNPACK #-} !Int64
                           {-# UNPACK #-} !Int64
                           {-# UNPACK #-} !Int64
                           {-# UNPACK #-} !Int64


data Sequencer  = Sequencer !Sequence
                            -- ^ cursor
                            ![Sequence]
                            -- ^ gating (aka consumer) sequences

data Barrier    = Barrier !Sequence
                          -- ^ cursor (must point to the same sequence as
                          -- the corresponding 'Sequencer')
                          ![Sequence]
                          -- ^ dependent sequences (optional)

data Consumer a = Consumer (a -> IO ())
                           -- ^ consuming function
                           {-# UNPACK #-} !Sequence
                           -- ^ consumer sequence

class Buffer a where
    consumeFrom :: a b
                -> Int64 -- ^ mod mask
                -> Barrier
                -> Consumer b
                -> IO ()

instance Buffer V.Vector where
    consumeFrom vec modm barr (Consumer fn sq) = do
        next  <- addAndGet' sq 1
        avail <- waitFor barr next

        let start = fromIntegral $ next .&. modm
            len   = fromIntegral $ avail - next
            (_,h) = V.splitAt start vec

        V.mapM_ fn h
        unless (V.length h >= len) $
            V.mapM_ fn (V.take (len - V.length h) vec)

        writeSeq sq avail
    {-# INLINE consumeFrom #-}

newtype MVector a = MVector { unMVector :: MV.IOVector a }
instance Buffer MVector where
    consumeFrom (MVector mvec) modm barr con = do
        vec <- V.unsafeFreeze mvec
        consumeFrom vec modm barr con
    {-# INLINE consumeFrom #-}


--
-- Value Constructors
--

newSequencer :: [Consumer a] -> IO Sequencer
newSequencer conss = do
    curs <- mkSeq

    return $ Sequencer curs (map gate conss)

    where
        gate (Consumer _ sq) = sq

newBarrier :: Sequencer -> [Sequence] -> Barrier
newBarrier (Sequencer curs _) = Barrier curs

newConsumer :: (a -> IO ()) -> IO (Consumer a)
newConsumer fn = do
    sq <- mkSeq

    return $ Consumer fn sq

--
-- Disruptor API
--

-- | Claim the given sequence value for publishing
claim :: Sequencer
      -> Int64      -- ^ sequence value to claim
      -> Int64      -- ^ buffer size
      -> IO Int64
claim (Sequencer _ gates) sq bufsize = await gates sq bufsize

-- | Claim the next available sequence for publishing
nextSeq :: Sequencer
        -> Sequence  -- ^ sequence to increment for next sequence value
        -> Int64     -- ^ buffer size
        -> IO Int64
nextSeq (Sequencer _ gates) sq bufsize = do
    --curr <- readIORef ref
    --await (curr + 1)

    next <- addAndGet' sq 1
    await gates next bufsize

-- | Wait for the given sequence value to be available for consumption
waitFor :: Barrier -> Int64 -> IO Int64
waitFor b@(Barrier sq deps) i = do
    avail <- if null deps then readSeq sq else minSeq deps

    if avail >= i
        then return avail
        else yield *> waitFor b i

-- | Make the given sequence visible to consumers
publish :: Sequencer -> Int64 -> IO ()
publish s@(Sequencer sq _) i = do
    let expected = i - 1 -- maybe support batch sizes instead of constant 1?
    curr <- readSeq sq

    if expected == curr
        then writeSeq sq i
        else publish s i


--
-- Util
--

consumerSeq :: Consumer a -> IO Int64
consumerSeq (Consumer _ sq) = readSeq sq
{-# INLINE consumerSeq #-}


--
-- Internal
--

minSeq :: [Sequence] -> IO Int64
minSeq ss = fromIntegral . minimum <$> mapM get ss

    where
        get = readIORef . unSeq
{-# INLINE minSeq #-}

await :: [Sequence] -> Int64 -> Int64 -> IO Int64
await gates n bufsize = do
    m <- minSeq gates
    if (n - bufsize <= m) then return n else await gates n bufsize
{-# INLINE await #-}

addAndGet :: IORef Int64 -> Int64 -> IO Int64
addAndGet ref delta = atomicModifyIORefCAS ref $ pair . (+delta)

    where
        pair x = (x, x)
{-# INLINE addAndGet #-}

addAndGet' :: Sequence -> Int64 -> IO Int64
addAndGet' = addAndGet . unSeq
{-# INLINE addAndGet' #-}

mkSeq :: IO Sequence
mkSeq = do
    ref <- newIORef (-1)
    return $ Sequence 7 7 7 7 7 7 7 ref 7 7 7 7 7 7 7
{-# INLINE mkSeq #-}

unSeq :: Sequence -> IORef Int64
unSeq (Sequence _ _ _ _ _ _ _ ref _ _ _ _ _ _ _) = ref
{-# INLINE unSeq #-}

readSeq :: Sequence -> IO Int64
readSeq = readIORef . unSeq
{-# INLINE readSeq #-}

writeSeq :: Sequence -> Int64 -> IO ()
writeSeq = writeIORef . unSeq
{-# INLINE writeSeq #-}


-- vim: set ts=4 sw=4 et:
