{-# LANGUAGE RecordWildCards #-}

module Data.RingBuffer
    ( Forkable (..)
    , newMultiProducerRingBuffer
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
import           Control.Monad                            (forM_, liftM, when)
import           Control.Monad.Catch                      (MonadMask (..),
                                                           finally)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.RingBuffer.RingBuffer               (RingBuffer, elemAt,
                                                           mkRingBuffer)
import qualified Data.RingBuffer.RingBuffer               as RB
import           Data.RingBuffer.Sequence
import           Data.RingBuffer.SequenceBarrier
import           Data.RingBuffer.Sequencer                (mkMultiProducerSequencer, mkSingleProducerSequencer)
import qualified Data.RingBuffer.Sequencer.MultiProducer  as Multi
import qualified Data.RingBuffer.Sequencer.SingleProducer as Single


data Consumer m a s
    = Consumer (a -> m ())
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

class Forkable m where
    fork :: m () -> m ThreadId

instance Forkable IO where
    fork = forkIO

data Disruptor a s = Disruptor (RingBuffer a s) [ThreadId] (IORef Bool)

{-
 - data Event = Event { a :: IORef Int, b :: IORef Int, c :: IORef Int }
 -
 -     newRingBuffer 32 (Event <$> newIORef 0 <*> newIORef 0 <*> newIORef 0)
 - >>= consumeWith (\ Event{..} -> modifyIORef' a (+1))
 - >>= andAlso     (\ Event{..} -> modifyIORef' b (*10))
 - >>= andThen     (\ Event{..} -> do
 -                        a' <- readIORef a
 -                        b' <- readIORef b
 -                        modifyIORef' c (a' + b'))
 - >>= start
 -}

newMultiProducerRingBuffer :: MonadIO m => Int -> m a -> m (RingBuffer a Multi.Sequencer)
newMultiProducerRingBuffer siz fill = do
    sqr <- mkMultiProducerSequencer siz []
    mkRingBuffer sqr fill

newSingleProducerRingBuffer :: MonadIO m => Int -> m a -> m (RingBuffer a Single.Sequencer)
newSingleProducerRingBuffer siz fill = do
    sqr <- mkSingleProducerSequencer siz []
    mkRingBuffer sqr fill

consumeWith :: MonadIO m => (a -> m ()) -> RingBuffer a s -> m (ConsumerGroup m a s)
consumeWith f b = do
    h <- mkConsumer b f []
    return $ ConsumerGroup b Nothing [h]

andAlso :: MonadIO m => (a -> m ()) -> ConsumerGroup m a s -> m (ConsumerGroup m a s)
andAlso f cg@ConsumerGroup{..} = do
    h <- mkConsumer rb f []
    return cg { hs = h : hs }

andThen :: MonadIO m => (a -> m ()) -> ConsumerGroup m a s -> m (ConsumerGroup m a s)
andThen f cg@ConsumerGroup{..} = do
    h <- mkConsumer rb f (map consumerSequence hs)
    return cg { hs = [h], pr = Just cg }


start :: (MonadIO m, Forkable m, MonadMask m) => ConsumerGroup m a s -> m (Disruptor a s)
start cg@ConsumerGroup{..} = do
    let rb' = RB.addGates rb (map consumerSequence hs)
    tids    <- startConsumers cg { rb = rb' }
    running <- liftIO $ newIORef True
    return $ Disruptor rb' tids running
  where
    startConsumers (ConsumerGroup rb' Nothing     cs) = mapM (run rb') cs
    startConsumers (ConsumerGroup rb' (Just prev) cs) = do
        t1 <- startConsumers prev { rb = rb' }
        t2 <- startConsumers $ ConsumerGroup  rb' Nothing cs
        return $ t1 ++ t2

stop :: MonadIO m => Disruptor a s -> m ()
stop (Disruptor _ tids ref) = liftIO $ do
    running <- liftIO $ atomicModifyIORef ref ((,) False)
    when running $
        mapM_ (liftIO . killThread) tids

publish :: MonadIO m => Disruptor a s -> (a -> m ()) -> m ()
publish (Disruptor rb _ _) = RB.publish rb

publishMany :: MonadIO m => Disruptor a s -> Int -> (a -> m ()) -> m ()
publishMany (Disruptor rb _ _) = RB.publishMany rb


--------------------------------------------------------------------------------
-- internal
--------------------------------------------------------------------------------

mkConsumer :: MonadIO m => RingBuffer a s -> (a -> m ()) -> [Sequence] -> m (Consumer m a s)
mkConsumer b f deps = do
    sq <- mkSequence
    return $ Consumer f sq (SequenceBarrier (RB.sequencer b) deps)

consumerSequence :: Consumer m a s -> Sequence
consumerSequence (Consumer _ s _) = s

run :: (MonadIO m, Forkable m, MonadMask m)
    => RingBuffer a s
    -> Consumer m a s
    -> m ThreadId
run buf (Consumer f sq bar) = fork loop
  where
    loop = do
        next  <- (+1) `liftM` readSequence sq
        avail <- waitFor bar next

        forM_ [next .. avail] (f . (buf `elemAt`))
            `finally` writeSequence sq avail
        loop

-- vim: set ts=4 sw=4 et:
