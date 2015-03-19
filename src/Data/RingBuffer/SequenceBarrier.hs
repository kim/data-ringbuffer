module Data.RingBuffer.SequenceBarrier
    ( SequenceBarrier (..)
    , waitFor
    )
where

import Control.Concurrent        (yield)
import Data.RingBuffer.Sequence
import Data.RingBuffer.Sequencer


data SequenceBarrier s
    = SequenceBarrier !(Sequencer s)
                      [Sequence]
                      -- ^ dependent sequences


waitFor :: SequenceBarrier s -> Int -> IO Int
waitFor barrier@(SequenceBarrier sqr deps) sq = do
    avail <- case deps of
        [] -> readSequence (cursor sqr)
        xs -> minimumSequence xs maxBound

    if avail >= sq
        then highestPublishedSequence sqr sq avail
        else yield >> waitFor barrier sq

-- vim: set ts=4 sw=4 et:
