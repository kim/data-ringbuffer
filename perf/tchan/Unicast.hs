module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception        (finally)
import Control.Monad            (unless)
import Data.Int

import Util

main :: IO ()
main = do
    chan  <- newTChanIO
    done  <- newEmptyMVar :: IO (MVar ())
    start <- now

    forkIO $ publishTChan chan 0
    forkIO $ consumeTChan chan 0 `finally` putMVar done ()

    takeMVar done >> now >>= printTiming iterations start

    where
        publishTChan chan i = unless (i > iterations)
            $ atomically (writeTChan chan i) >> publishTChan chan (i + 1)

        consumeTChan chan i = unless (i > iterations)
            $ atomically (readTChan chan) >> consumeTChan chan (i + 1)


-- vim: set ts=4 sw=4 et:
