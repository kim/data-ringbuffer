{-# LANGUAGE RecordWildCards #-}

module Data.RingBuffer
    ( newMultiProducerRingBuffer
    , newSingleProducerRingBuffer
    , consumeWith
    , andAlso
    , andThen
    , start
    , stop
    , publish
    , publishMany
    )
where

import           Control.Concurrent
import           Control.Monad                   (forM_, liftM, when)
import           Control.Monad.Catch             (finally)
import           Data.IORef
import           Data.RingBuffer.RingBuffer      (RingBuffer, elemAt,
                                                  mkRingBuffer)
import qualified Data.RingBuffer.RingBuffer      as RB
import           Data.RingBuffer.Sequence
import           Data.RingBuffer.SequenceBarrier
import           Data.RingBuffer.Sequencer       ( SingleProducer
                                                 , MultiProducer
                                                 , mkMultiProducerSequencer
                                                 , mkSingleProducerSequencer
                                                 )


data Consumer m a s
    = Consumer (a -> IO ())
               -- ^ event processing action
               !Sequence
               -- ^ tracks which events were consumed by this 'Consumer'
               !(SequenceBarrier s)
               -- ^ barrier tracking producers and/or prerequisite handlers

data ConsumerGroup m a s = ConsumerGroup
    { rb :: RingBuffer a s
    , pr :: Maybe (ConsumerGroup m a s)
    , hs :: [Consumer m a s]
    }

data Disruptor a s = Disruptor (RingBuffer a s) [ThreadId] (IORef Bool)


newMultiProducerRingBuffer :: Int -> IO a -> IO (RingBuffer a MultiProducer)
newMultiProducerRingBuffer siz fill = do
    sqr <- mkMultiProducerSequencer siz []
    mkRingBuffer sqr fill

newSingleProducerRingBuffer :: Int -> IO a -> IO (RingBuffer a SingleProducer)
newSingleProducerRingBuffer siz fill = do
    sqr <- mkSingleProducerSequencer siz []
    mkRingBuffer sqr fill

consumeWith :: (a -> IO ()) -> RingBuffer a s -> IO (ConsumerGroup m a s)
consumeWith f b = do
    h <- mkConsumer b f []
    return $ ConsumerGroup b Nothing [h]

andAlso :: (a -> IO ()) -> ConsumerGroup m a s -> IO (ConsumerGroup m a s)
andAlso f cg@ConsumerGroup{..} = do
    h <- mkConsumer rb f []
    return cg { hs = h : hs }

andThen :: (a -> IO ()) -> ConsumerGroup m a s -> IO (ConsumerGroup m a s)
andThen f cg@ConsumerGroup{..} = do
    h <- mkConsumer rb f (map consumerSequence hs)
    return cg { hs = [h], pr = Just cg }


start :: ConsumerGroup m a s -> IO (Disruptor a s)
start cg@ConsumerGroup{..} = do
    let rb' = RB.addGates rb (map consumerSequence hs)
    tids    <- startConsumers cg { rb = rb' }
    running <- newIORef True
    return $ Disruptor rb' tids running
  where
    startConsumers (ConsumerGroup rb' Nothing     cs) = mapM (run rb') cs
    startConsumers (ConsumerGroup rb' (Just prev) cs) = do
        t1 <- startConsumers prev { rb = rb' }
        t2 <- startConsumers $ ConsumerGroup  rb' Nothing cs
        return $ t1 ++ t2

stop :: Disruptor a s -> IO ()
stop (Disruptor _ tids ref) = do
    running <- atomicModifyIORef ref ((,) False)
    when running $
        mapM_ killThread tids

publish :: Disruptor a s -> (a -> IO ()) -> IO ()
publish (Disruptor rb _ _) = RB.publish rb

publishMany :: Disruptor a s -> Int -> (a -> IO ()) -> IO ()
publishMany (Disruptor rb _ _) = RB.publishMany rb


--------------------------------------------------------------------------------
-- internal
--------------------------------------------------------------------------------

mkConsumer :: RingBuffer a s -> (a -> IO ()) -> [Sequence] -> IO (Consumer m a s)
mkConsumer b f deps = do
    sq <- mkSequence
    return $ Consumer f sq (SequenceBarrier (RB.sequencer b) deps)

consumerSequence :: Consumer m a s -> Sequence
consumerSequence (Consumer _ s _) = s

run :: RingBuffer a s -> Consumer m a s -> IO ThreadId
run buf (Consumer f sq bar) = forkIO loop
  where
    loop = do
        next  <- (+1) `liftM` readSequence sq
        avail <- waitFor bar next

        forM_ [next .. avail] (f . (buf `elemAt`))
            `finally` writeSequence sq avail
        loop

-- vim: set ts=4 sw=4 et:
