{-# LANGUAGE BangPatterns #-}

module Data.RingBuffer.Sequencer.SingleProducer
    ( Sequencer (..)
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

import Control.Concurrent                 (yield)
import Control.Monad                      (liftM, when)
import Control.Monad.IO.Class
import Data.IORef
import Data.RingBuffer.Sequence
import Data.RingBuffer.Sequencer.Internal


data Sequencer
    = Sequencer !Sequence
                -- ^ cursor
                !Int
                -- ^ buffer size
                [Sequence]
                -- ^ "gating" sequences
                (IORef Int)
                -- ^ next value
                (IORef Int)
                -- ^ cached value


mkSequencer :: MonadIO m => Int -> [Sequence] -> m Sequencer
mkSequencer size gating = do
    sq  <- mkSequence
    nxt <- liftIO $ newIORef (-1)
    chd <- liftIO $ newIORef (-1)
    return $ Sequencer sq size' gating nxt chd
  where
    size' = ceilNextPowerOfTwo size

bufferSize :: Sequencer -> Int
bufferSize (Sequencer _ s _ _ _) = s
{-# INLINABLE bufferSize #-}

cursor :: Sequencer -> Sequence
cursor (Sequencer c _ _ _ _) = c
{-# INLINABLE cursor #-}

addGates :: Sequencer -> [Sequence] -> Sequencer
addGates (Sequencer sq siz gates nxt cache) gates' =
    Sequencer sq siz (gates ++ gates') nxt cache
{-# INLINABLE addGates #-}

next :: MonadIO m => Sequencer -> Int -> m Int
next (Sequencer _ s gs nxt cache) n = do
    nextValue <- liftIO $ readIORef nxt
    gate      <- liftIO $ readIORef cache

    let nextSequence = nextValue + n
        wrap         = nextSequence - s

    when (wrap > gate || gate > nextValue) $ do
        loop wrap (minimumSequence gs nextValue)

    liftIO $ writeIORef nxt nextSequence

    return nextSequence
  where
    loop !wrap m = do
        minsq <- m
        if wrap > minsq
            then do
                liftIO yield
                loop wrap m
            else
                liftIO $ writeIORef cache minsq
{-# INLINABLE next #-}

publish :: MonadIO m => Sequencer -> Int -> m ()
publish (Sequencer c _ _ _ _) = writeSequence c
{-# INLINABLE publish #-}

publishRange :: MonadIO m => Sequencer -> Int -> Int -> m ()
publishRange s _ hi = publish s hi
{-# INLINABLE publishRange #-}

isAvailable :: MonadIO m => Sequencer -> Int -> m Bool
isAvailable (Sequencer c _ _ _ _) s = (s <=) `liftM` readSequence c
{-# INLINABLE isAvailable #-}

highestPublishedSequence :: MonadIO m => Sequencer -> Int -> Int -> m Int
highestPublishedSequence _ _ x = return x -- orly?!
{-# INLINABLE highestPublishedSequence #-}


-- vim: set ts=4 sw=4 et:
