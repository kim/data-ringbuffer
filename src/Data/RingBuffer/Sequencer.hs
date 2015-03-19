{-# LANGUAGE GADTs #-}

module Data.RingBuffer.Sequencer
    ( Sequencer
    , SingleProducer
    , MultiProducer
    , mkMultiProducerSequencer
    , mkSingleProducerSequencer
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

import           Control.Monad                            (liftM)
import           Data.RingBuffer.Sequence
import qualified Data.RingBuffer.Sequencer.MultiProducer  as Multi
import qualified Data.RingBuffer.Sequencer.SingleProducer as Single


type SingleProducer = Single.Sequencer
type MultiProducer  = Multi.Sequencer

data Sequencer a where
    MultiProducerSequencer  :: Multi.Sequencer  -> Sequencer MultiProducer
    SingleProducerSequencer :: Single.Sequencer -> Sequencer SingleProducer


mkMultiProducerSequencer :: Int -> [Sequence] -> IO (Sequencer Multi.Sequencer)
mkMultiProducerSequencer size = liftM MultiProducerSequencer . Multi.mkSequencer size

mkSingleProducerSequencer :: Int -> [Sequence] -> IO (Sequencer Single.Sequencer)
mkSingleProducerSequencer size = liftM SingleProducerSequencer . Single.mkSequencer size

bufferSize :: Sequencer a -> Int
bufferSize (SingleProducerSequencer s) = Single.bufferSize s
bufferSize (MultiProducerSequencer  s) = Multi.bufferSize s
{-# SPECIALISE INLINE bufferSize :: Sequencer SingleProducer -> Int #-}
{-# SPECIALISE INLINE bufferSize :: Sequencer MultiProducer  -> Int #-}

cursor :: Sequencer a -> Sequence
cursor (SingleProducerSequencer s) = Single.cursor s
cursor (MultiProducerSequencer  s) = Multi.cursor s
{-# SPECIALISE INLINE cursor :: Sequencer SingleProducer -> Sequence #-}
{-# SPECIALISE INLINE cursor :: Sequencer MultiProducer  -> Sequence #-}

addGates :: Sequencer a -> [Sequence] -> Sequencer a
addGates (SingleProducerSequencer s) = SingleProducerSequencer . Single.addGates s
addGates (MultiProducerSequencer  s) = MultiProducerSequencer  . Multi.addGates s
{-# SPECIALISE INLINE addGates :: Sequencer SingleProducer -> [Sequence] -> Sequencer SingleProducer #-}
{-# SPECIALISE INLINE addGates :: Sequencer MultiProducer  -> [Sequence] -> Sequencer MultiProducer #-}

next :: Sequencer a -> Int -> IO Int
next (SingleProducerSequencer s) = Single.next s
next (MultiProducerSequencer  s) = Multi.next s
{-# SPECIALISE INLINE next :: Sequencer SingleProducer -> Int -> IO Int #-}
{-# SPECIALISE INLINE next :: Sequencer MultiProducer  -> Int -> IO Int #-}

publish :: Sequencer a -> Int -> IO ()
publish (SingleProducerSequencer s) = Single.publish s
publish (MultiProducerSequencer  s) = Multi.publish s
{-# SPECIALISE INLINE publish :: Sequencer SingleProducer -> Int -> IO () #-}
{-# SPECIALISE INLINE publish :: Sequencer MultiProducer  -> Int -> IO () #-}

publishRange :: Sequencer a -> Int -> Int -> IO ()
publishRange (SingleProducerSequencer s) = Single.publishRange s
publishRange (MultiProducerSequencer  s) = Multi.publishRange s
{-# SPECIALISE INLINE publishRange :: Sequencer SingleProducer -> Int -> Int -> IO () #-}
{-# SPECIALISE INLINE publishRange :: Sequencer MultiProducer  -> Int -> Int -> IO () #-}

isAvailable :: Sequencer a -> Int -> IO Bool
isAvailable (SingleProducerSequencer s) = Single.isAvailable s
isAvailable (MultiProducerSequencer  s) = Multi.isAvailable s
{-# SPECIALISE INLINE isAvailable :: Sequencer SingleProducer -> Int -> IO Bool #-}
{-# SPECIALISE INLINE isAvailable :: Sequencer MultiProducer  -> Int -> IO Bool #-}

highestPublishedSequence :: Sequencer a -> Int -> Int -> IO Int
highestPublishedSequence (SingleProducerSequencer s) = Single.highestPublishedSequence s
highestPublishedSequence (MultiProducerSequencer  s) = Multi.highestPublishedSequence s
{-# SPECIALISE INLINE highestPublishedSequence :: Sequencer SingleProducer -> Int -> Int -> IO Int #-}
{-# SPECIALISE INLINE highestPublishedSequence :: Sequencer MultiProducer  -> Int -> Int -> IO Int #-}
