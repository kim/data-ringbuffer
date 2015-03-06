module Data.RingBuffer.Sequence
    ( Sequence
    , mkSequence
    , readSequence
    , writeSequence
    , casSequence
    , minimumSequence
    )
where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Atomics
import Data.IORef
import Data.List              (foldl')


newtype Sequence = Sequence (IORef Int)


mkSequence :: MonadIO m => m Sequence
mkSequence = liftIO $ Sequence <$> newIORef (-1)

readSequence :: MonadIO m => Sequence -> m Int
readSequence (Sequence ref) = liftIO $ readIORef ref

writeSequence :: MonadIO m => Sequence -> Int -> m ()
writeSequence (Sequence ref) = liftIO . writeIORef ref

casSequence :: MonadIO m => Sequence -> Int -> Int -> m Bool
casSequence (Sequence ref) old new = liftIO . atomicModifyIORefCAS ref $ \ x ->
    if x == old then (new, True) else (old, False)

minimumSequence :: MonadIO m => [Sequence] -> Int -> m Int
minimumSequence [] def = return def
minimumSequence ss def = return . foldl' min def =<< mapM readSequence ss


-- vim: set ts=4 sw=4 et:
