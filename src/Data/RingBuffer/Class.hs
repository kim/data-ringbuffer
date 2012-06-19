module Data.RingBuffer.Class
    ( RingBuffer(..)
    )
where

import           Data.RingBuffer.Types


class RingBuffer a where
    newRingBuffer :: Int -> b -> IO (a b)

    consumeFrom :: a b
                -> Int
                -- ^ mod mask
                -> Barrier
                -> Consumer b
                -> IO ()

    publishTo :: a b
              -> Int
              -- ^ mod mask
              -> Sequencer
              -> Int
              -- ^ position to claim
              -> b
              -> IO ()

    batchPublishTo :: a b
                   -> Int
                   -- ^ mod mask
                   -> Sequencer
                   -> Int
                   -- ^ position to claim
                   -- must fit length b
                   -> [b]
                   -> IO ()

    concPublishTo :: a b
                  -> Int
                  -- ^ mod mask
                  -> Sequencer
                  -> Sequence
                  -- ^ sequence to incement for next position
                  -> b
                  -> IO ()

    concBatchPublishTo :: a b
                       -> Int
                       -- ^ mod mask
                       -> Sequencer
                       -> Sequence
                       -- ^ sequence to increment by requested batch
                       -> Int
                       -- ^ batch size
                       -- must fit length b
                       -> [b]
                       -> IO ()


-- vim: set ts=4 sw=4 et:
