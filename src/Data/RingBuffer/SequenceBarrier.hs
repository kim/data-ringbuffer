module Data.RingBuffer.SequenceBarrier
    ( SequenceBarrier (..)
    , waitFor
    )
where

import Control.Concurrent        (yield)
import Control.Monad.IO.Class
import Data.RingBuffer.Sequence
import Data.RingBuffer.Sequencer


data SequenceBarrier
    = SequenceBarrier !Sequencer
                      [Sequence]
                      -- ^ dependent sequences


waitFor :: MonadIO m => SequenceBarrier -> Int -> m Int
waitFor barrier@(SequenceBarrier sqr deps) sq = do
    avail <- case deps of
        [] -> readSequence (cursor sqr)
        xs -> minimumSequence xs maxBound

    if avail >= sq
        then highestPublishedSequence sqr sq avail
        else liftIO yield >> waitFor barrier sq

-- vim: set ts=4 sw=4 et:
