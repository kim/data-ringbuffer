module Data.RingBuffer.RingBuffer
    ( RingBuffer
    , mkRingBuffer
    , sequencer
    , addGates
    , publish
    , publishMany
    , elemAt
    )
where

import           Control.Monad.IO.Class
import           Data.Bits
import           Data.Foldable             (forM_)
import           Data.RingBuffer.Sequence  (Sequence)
import           Data.RingBuffer.Sequencer (Sequencer, bufferSize)
import qualified Data.RingBuffer.Sequencer as S
import           Data.Vector               (Vector, (!))
import qualified Data.Vector               as V


data RingBuffer a s
    = RingBuffer !Int
                 -- ^ index mask
                 (Vector a)
                 -- ^ entries
                 !(Sequencer s)

mkRingBuffer :: MonadIO m => Sequencer s -> m a -> m (RingBuffer a s)
mkRingBuffer sqr fill = do
    vs <- V.replicateM (bufferSize sqr) fill
    return $ RingBuffer (bufferSize sqr - 1) vs sqr

sequencer :: RingBuffer a s -> Sequencer s
sequencer (RingBuffer _ _ s) = s

addGates :: RingBuffer a s -> [Sequence] -> RingBuffer a s
addGates (RingBuffer msk vs sqr) = RingBuffer msk vs . S.addGates sqr

publish :: MonadIO m => RingBuffer a s -> (a -> m ()) -> m ()
publish (RingBuffer msk vs sqr) update = do
    next <- S.next sqr 1
    update $ vs ! (next .&. msk)
    S.publish sqr next

publishMany :: MonadIO m => RingBuffer a s -> Int -> (a -> m ()) -> m ()
publishMany (RingBuffer msk vs sqr) n update = do
    next <- S.next sqr n
    forM_ [next - n - 1 .. next] $ \ i ->
        update $ vs ! (i .&. msk)
    S.publishRange sqr (next - n - 1) next

elemAt :: RingBuffer a s -> Int -> a
elemAt (RingBuffer msk vs _) i = vs ! (i .&. msk)
{-# INLINABLE elemAt #-}


-- vim: set ts=4 sw=4 et:
