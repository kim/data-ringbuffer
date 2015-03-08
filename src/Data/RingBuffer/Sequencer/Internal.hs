{-# LANGUAGE BangPatterns #-}

module Data.RingBuffer.Sequencer.Internal
  ( ceilNextPowerOfTwo
  , log2
  )
where

import Data.Bits


ceilNextPowerOfTwo :: Int -> Int
ceilNextPowerOfTwo i = shiftL 1 (32 - numberOfLeadingZeros (i - 1))
{-# INLINABLE ceilNextPowerOfTwo #-}

numberOfLeadingZeros :: Int -> Int
numberOfLeadingZeros i = nlz i 1
  where
    nlz 0 _ = 32
    nlz i' n | shiftR i' 16 == 0 = nlz (shiftL i' 16) (n + 16)
             | shiftR i' 24 == 0 = nlz (shiftL i'  8) (n +  8)
             | shiftR i' 28 == 0 = nlz (shiftL i'  4) (n +  4)
             | shiftR i' 30 == 0 = nlz (shiftL i'  2) (n +  2)
             | otherwise = n - shiftR i' 31

log2 :: Int -> Int
log2 = loop 0
  where
    loop !r i = let i' = shiftR i 1
                 in if i' == 0 then r else loop (r+1) i'
{-# INLINABLE log2 #-}


-- vim: set ts=4 sw=4 et:
