module Data.RingBuffer.Sequencer
    ( Sequencer
    , mkSequencer
    , bufferSize
    , cursor
    , addGates
    , next
    , publish
    , publishRange
    , isAvailable
    , highestPublishedSequence
    )
where

import           Control.Concurrent          (yield)
import           Control.Monad               (liftM)
import           Control.Monad.IO.Class
import           Data.Bits                   hiding (shift)
import           Data.RingBuffer.Sequence
import qualified Data.Vector.Unboxed.Mutable as MV


data Sequencer
    = Sequencer !Sequence
                -- ^ cursor
                !Int
                -- ^ buffer size
                [Sequence]
                -- ^ "gating" sequences tracking concurrent producers
                !Sequence
                -- ^ min sequence cache
                (MV.IOVector Int)
                -- ^ available buffer
                !Int
                -- ^ mask
                !Int
                -- ^ shift


mkSequencer :: MonadIO m => Int -> [Sequence] -> m Sequencer
mkSequencer size gating = do
    sq <- mkSequence
    mn <- mkSequence
    ab <- liftIO $ MV.replicate size' (-1)
    return $ Sequencer sq size' gating mn ab (size' - 1) (log2 size')
  where
    size' = ceilNextPowerOfTwo size

bufferSize :: Sequencer -> Int
bufferSize (Sequencer _ s _ _ _ _ _) = s
{-# INLINABLE bufferSize #-}

cursor :: Sequencer -> Sequence
cursor (Sequencer c _ _ _ _ _ _) = c
{-# INLINABLE cursor #-}

addGates :: Sequencer -> [Sequence] -> Sequencer
addGates (Sequencer sq siz gates cache ab msk shft) gates' =
    Sequencer sq siz (gates ++ gates') cache ab msk shft

next :: MonadIO m => Sequencer -> Int -> m Int
next sq@(Sequencer c s gs mcache _ _ _) n = do
    curr <- readSequence c

    let nxt  = curr + n
        wrap = nxt - s

    mingate <- readSequence mcache

    if wrap > mingate || mingate > curr
        then do
            mingate' <- minimumSequence gs curr
            if wrap > mingate'
                then do
                    liftIO yield
                    next sq n
                else do
                    writeSequence mcache mingate'
                    next sq n
        else do
            cas'd <- casSequence c curr nxt
            if cas'd
                then return nxt
                else next sq n

publish :: MonadIO m => Sequencer -> Int -> m ()
publish = setAvailable

publishRange :: MonadIO m => Sequencer -> Int -> Int -> m ()
publishRange s lo hi = go lo
  where
    go i | i <= hi   = setAvailable s i >> go (i+i)
         | otherwise = return ()

isAvailable :: MonadIO m => Sequencer -> Int -> m Bool
isAvailable (Sequencer _ _ _ _ avail mask shift) sq = liftIO $
    (== shiftR sq shift) `liftM` MV.unsafeRead avail (sq .&. mask)

highestPublishedSequence :: MonadIO m => Sequencer -> Int -> Int -> m Int
highestPublishedSequence _ lo hi | lo > hi = error "invalid lower bound"
highestPublishedSequence s lo hi           = go lo
  where
    go i | i <= hi = do
            avail <- isAvailable s i
            if not avail then return (i - 1) else go (i+1)
         | otherwise = return hi


--------------------------------------------------------------------------------
-- internal
--------------------------------------------------------------------------------

setAvailable :: MonadIO m => Sequencer -> Int -> m ()
setAvailable (Sequencer _ _ _ _ avail mask shift) sq = liftIO $
    MV.unsafeWrite avail (sq .&. mask) (shiftR sq shift)

ceilNextPowerOfTwo :: Int -> Int
ceilNextPowerOfTwo i = shiftL 1 (32 - numberOfLeadingZeros (i - 1))

numberOfLeadingZeros :: Int -> Int
numberOfLeadingZeros i = nlz i 1
  where
    nlz 0 _ = 32
    nlz i' n | shiftR i' 16 == 0 = nlz (shiftL i' 16) (n + 16)
             | shiftR i' 24 == 0 = nlz (shiftL i'  8) (n +  8)
             | shiftR i' 28 == 0 = nlz (shiftL i'  4) (n +  4)
             | shiftR i' 30 == 0 = nlz (shiftL i'  2) (n +  2)
             | otherwise = n - shiftR i' 31

log2 :: Int -> Int
log2 = loop 0
  where
    loop r i = let i' = shiftR i 1
                in if i' == 0 then r else loop (r+1) i'

-- vim: set ts=4 sw=4 et:
