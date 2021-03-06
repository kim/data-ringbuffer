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

import           Data.Bits
import           Data.Foldable             (forM_)
import           Data.RingBuffer.Sequence  (Sequence)
import           Data.RingBuffer.Sequencer (Sequencer, bufferSize)
import qualified Data.RingBuffer.Sequencer as S
import           Data.Vector               (Vector, unsafeIndex)
import qualified Data.Vector               as V


data RingBuffer a s
    = RingBuffer !Int
                 -- ^ index mask
                 !(Vector a)
                 -- ^ entries
                 !(Sequencer s)

mkRingBuffer :: Sequencer s -> IO a -> IO (RingBuffer a s)
mkRingBuffer sqr fill = do
    vs <- V.replicateM (bufferSize sqr) fill
    return $ RingBuffer (bufferSize sqr - 1) vs sqr

sequencer :: RingBuffer a s -> Sequencer s
sequencer (RingBuffer _ _ s) = s
{-# INLINABLE sequencer #-}

addGates :: RingBuffer a s -> [Sequence] -> RingBuffer a s
addGates (RingBuffer msk vs sqr) = RingBuffer msk vs . S.addGates sqr

publish :: RingBuffer a s -> (a -> IO ()) -> IO ()
publish (RingBuffer msk vs sqr) update = do
    next <- S.next sqr 1
    update $ vs `unsafeIndex` (next .&. msk)
    S.publish sqr next

publishMany :: RingBuffer a s -> Int -> (a -> IO ()) -> IO ()
publishMany (RingBuffer msk vs sqr) n update = do
    next <- S.next sqr n
    forM_ [next - n - 1 .. next] $ \ i ->
        update $ vs `unsafeIndex` (i .&. msk)
    S.publishRange sqr (next - n - 1) next

elemAt :: RingBuffer a s -> Int -> a
elemAt (RingBuffer msk vs _) i = vs `unsafeIndex` (i .&. msk)
{-# INLINABLE elemAt #-}


-- vim: set ts=4 sw=4 et:
