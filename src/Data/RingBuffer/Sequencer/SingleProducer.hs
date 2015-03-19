{-# LANGUAGE BangPatterns  #-}

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
import Data.RingBuffer.Sequence
import Data.RingBuffer.Sequencer.Internal


data Sequencer
    = Sequencer !Sequence
                -- ^ cursor
                !Int
                -- ^ buffer size
                [Sequence]
                -- ^ "gating" sequences
                !Sequence
                -- ^ next value
                !Sequence
                -- ^ cached value


mkSequencer :: Int -> [Sequence] -> IO Sequencer
mkSequencer size gating = do
    sq  <- mkSequence
    nxt <- mkSequence
    chd <- mkSequence
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

next :: Sequencer -> Int -> IO Int
next (Sequencer _ s gs nxt cache) n = {-# SCC "next" #-} do
    nextValue <- readSequence nxt
    gate      <- readSequence cache

    let nextSequence = nextValue + n
        wrap         = nextSequence - s

    when (wrap > gate || gate > nextValue) $
        loop wrap (minimumSequence gs nextValue)

    writeSequence nxt nextSequence

    return nextSequence
  where
    loop !wrap m = {-# SCC "next.loop" #-} do
        minsq <- m
        if wrap > minsq
            then do
                {-# SCC "next.loop.yield" #-} yield
                loop wrap m
            else
                writeSequence cache minsq
{-# INLINABLE next #-}

publish :: Sequencer -> Int -> IO ()
publish (Sequencer c _ _ _ _) = writeSequence c
{-# INLINABLE publish #-}

publishRange :: Sequencer -> Int -> Int -> IO ()
publishRange s _ = publish s
{-# INLINABLE publishRange #-}

isAvailable :: Sequencer -> Int -> IO Bool
isAvailable (Sequencer c _ _ _ _) s = (s <=) `liftM` readSequence c
{-# INLINABLE isAvailable #-}

highestPublishedSequence :: Sequencer -> Int -> Int -> IO Int
highestPublishedSequence _ _ = return -- orly?!
{-# INLINABLE highestPublishedSequence #-}


-- vim: set ts=4 sw=4 et:
