module Main where

import Control.Concurrent  ( newEmptyMVar
                           , putMVar
                           , takeMVar
                           , forkIO
                           )
import Criterion           (bench)
import Criterion.Main      (defaultMain)
import Data.Int
import Data.IORef
import Data.CAS


data Struct = Struct !(IORef Int64) !(IORef Int64)

data PaddedLong = PaddedLong {-# UNPACK #-} !Int64
                             {-# UNPACK #-} !Int64
                             {-# UNPACK #-} !Int64
                             {-# UNPACK #-} !Int64
                             {-# UNPACK #-} !Int64
                             {-# UNPACK #-} !Int64
                             {-# UNPACK #-} !Int64
                             !(IORef Int64)
                             {-# UNPACK #-} !Int64
                             {-# UNPACK #-} !Int64
                             {-# UNPACK #-} !Int64
                             {-# UNPACK #-} !Int64
                             {-# UNPACK #-} !Int64
                             {-# UNPACK #-} !Int64
                             {-# UNPACK #-} !Int64

data Struct' = Struct' !PaddedLong !PaddedLong


unpadded :: Int64 -> IO ()
unpadded i = do
    ref1 <- newIORef 0
    ref2 <- newIORef 0

    lck1 <- newEmptyMVar
    lck2 <- newEmptyMVar

    let s = Struct ref1 ref2

    forkIO $ mapM_ (\_ -> incA s) [0..i] >> putMVar lck1 ()
    forkIO $ mapM_ (\_ -> incB s) [0..i] >> putMVar lck2 ()

    mapM_ takeMVar [lck1,lck2]

    where
        incA (Struct a _) = atomicModifyIORefCAS a $ pair . (+1)
        incB (Struct _ b) = atomicModifyIORefCAS b $ pair . (+1)

        pair x = (x, x)


padded :: Int64 -> IO ()
padded i = do
    ref1 <- newIORef 0
    ref2 <- newIORef 0

    lck1 <- newEmptyMVar
    lck2 <- newEmptyMVar

    let s = Struct' (mkPad ref1) (mkPad ref2)

    forkIO $ mapM_ (\_ -> incA s) [0..i] >> putMVar lck1 ()
    forkIO $ mapM_ (\_ -> incB s) [0..i] >> putMVar lck2 ()

    mapM_ takeMVar [lck1,lck2]

    where
        incA (Struct' (PaddedLong _ _ _ _ _ _ _ a _ _ _ _ _ _ _) _) =
            atomicModifyIORefCAS a $ pair . (+1)
        incB (Struct' _ (PaddedLong _ _ _ _ _ _ _ b _ _ _ _ _ _ _)) =
            atomicModifyIORefCAS b $ pair . (+1)

        pair x = (x, x)

        mkPad ref = PaddedLong 7 7 7 7 7 7 7 ref 7 7 7 7 7 7 7


main :: IO ()
main = defaultMain [ bench "unpadded" $ unpadded 1000000
                   , bench "padded"   $ padded   1000000
                   ]


-- vim: set ts=4 sw=4 et:
