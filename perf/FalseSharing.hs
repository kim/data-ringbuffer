module FalseSharing
    ( unpadded
    , padded
    )
where

import Control.Concurrent  ( newEmptyMVar
                           , putMVar
                           , takeMVar
                           , forkIO
                           )
import Data.IORef
import Data.Atomics


data Struct = Struct !(IORef Int) !(IORef Int)

data PaddedLong = PaddedLong {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                             !(IORef Int)
                             {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int

data Struct' = Struct' !PaddedLong !PaddedLong


unpadded :: Int -> IO ()
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


padded :: Int -> IO ()
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


-- vim: set ts=4 sw=4 et:
