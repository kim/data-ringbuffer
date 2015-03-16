{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.RingBuffer.Sequence
    ( Sequence
    , mkSequence
    , readSequence
    , writeSequence
    , casSequence
    , minimumSequence
    )
where

import Data.Atomics.Internal
import Data.List              (foldl')
#if MIN_VERSION_base(4,7,0)
import           GHC.Base           hiding ((==#))
import qualified GHC.PrimopWrappers as GPW
#else
import GHC.Base
#endif

-- GHC 7.8 changed some primops
#if MIN_VERSION_base(4,7,0)
(==#) :: Int# -> Int# -> Bool
(==#) x y = case x GPW.==# y of { 0# -> False; _ -> True }
#endif

#include "MachDeps.h"
#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif


data Sequence = Sequence (MutableByteArray# RealWorld)


mkSequence :: IO Sequence
mkSequence = do
    raw <- mkRaw
    writeSequence raw (-1)
    return raw
{-# INLINABLE mkSequence #-}

mkRaw :: IO Sequence
mkRaw = IO $ \ s ->
    case newPinnedByteArray# size s of
        (# s', arr #) -> (# s', Sequence arr #)
  where
    !(I# size) = SIZEOF_HSINT * 15
{-# INLINABLE mkRaw #-}

readSequence :: Sequence -> IO Int
readSequence (Sequence arr) = IO $ \ s ->
    case readIntArray# arr 7# s of
        (# s', i #) -> (# s', I# i #)
{-# INLINABLE readSequence #-}

writeSequence :: Sequence -> Int -> IO ()
writeSequence (Sequence arr) (I# i) = IO $ \ s ->
    case writeIntArray# arr 7# i s of
        s' -> (# s', () #)
{-# INLINABLE writeSequence #-}

casSequence :: Sequence -> Int -> Int -> IO Bool
casSequence (Sequence arr#) (I# old#) (I# new#) = IO $ \ s1# ->
    let (# s2#, res# #) = casIntArray# arr# 7# old# new# s1#
     in case res# ==# old# of
        False -> (# s2#, False #)
        True  -> (# s2#, True  #)
{-# INLINABLE casSequence #-}

minimumSequence :: [Sequence] -> Int -> IO Int
minimumSequence [] def = return def
minimumSequence ss def = return . foldl' min def =<< mapM readSequence ss
{-# INLINABLE minimumSequence #-}


-- vim: set ts=4 sw=4 et:
