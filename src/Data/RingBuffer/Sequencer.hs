{-# LANGUAGE GADTs #-}

module Data.RingBuffer.Sequencer
    ( Sequencer
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
import           Control.Monad.IO.Class
import           Data.RingBuffer.Sequence
import qualified Data.RingBuffer.Sequencer.MultiProducer  as Multi
import qualified Data.RingBuffer.Sequencer.SingleProducer as Single


type SingleProducer = Single.Sequencer
type MultiProducer  = Multi.Sequencer

data Sequencer a where
    MultiProducerSequencer  :: Multi.Sequencer  -> Sequencer MultiProducer
    SingleProducerSequencer :: Single.Sequencer -> Sequencer SingleProducer


mkMultiProducerSequencer :: MonadIO m => Int -> [Sequence] -> m (Sequencer Multi.Sequencer)
mkMultiProducerSequencer size = liftM MultiProducerSequencer . Multi.mkSequencer size

mkSingleProducerSequencer :: MonadIO m => Int -> [Sequence] -> m (Sequencer Single.Sequencer)
mkSingleProducerSequencer size = liftM SingleProducerSequencer . Single.mkSequencer size

bufferSize :: Sequencer a -> Int
bufferSize (SingleProducerSequencer s) = Single.bufferSize s
bufferSize (MultiProducerSequencer s)  = Multi.bufferSize s
{-# INLINABLE bufferSize #-}

cursor :: Sequencer a -> Sequence
cursor (SingleProducerSequencer s) = Single.cursor s
cursor (MultiProducerSequencer s)  = Multi.cursor s
{-# INLINABLE cursor #-}

addGates :: Sequencer a -> [Sequence] -> Sequencer a
addGates (SingleProducerSequencer s) = SingleProducerSequencer . Single.addGates s
addGates (MultiProducerSequencer s)  = MultiProducerSequencer  . Multi.addGates s

next :: MonadIO m => Sequencer a -> Int -> m Int
next (SingleProducerSequencer s) = Single.next s
next (MultiProducerSequencer s)  = Multi.next s

publish :: MonadIO m => Sequencer a -> Int -> m ()
publish (SingleProducerSequencer s) = Single.publish s
publish (MultiProducerSequencer s)  = Multi.publish s

publishRange :: MonadIO m => Sequencer a -> Int -> Int -> m ()
publishRange (SingleProducerSequencer s) = Single.publishRange s
publishRange (MultiProducerSequencer s)  = Multi.publishRange s

isAvailable :: MonadIO m => Sequencer a -> Int -> m Bool
isAvailable (SingleProducerSequencer s) = Single.isAvailable s
isAvailable (MultiProducerSequencer s)  = Multi.isAvailable s

highestPublishedSequence :: MonadIO m => Sequencer a -> Int -> Int -> m Int
highestPublishedSequence (SingleProducerSequencer s) = Single.highestPublishedSequence s
highestPublishedSequence (MultiProducerSequencer s)  = Multi.highestPublishedSequence s
