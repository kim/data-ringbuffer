module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception        (finally)
import Control.Monad            (unless)
import Criterion                (bench)
import Criterion.Main           (defaultMain)
import Data.Int

import Util

run :: Int64 -> IO ()
run iterations = do
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

main :: IO ()
main = defaultMain [ bench "multicast 3000"    $ run 3000
                   , bench "multicast 30000"   $ run 30000
                   , bench "multicast 3000000" $ run 3000000
                   ]

-- vim: set ts=4 sw=4 et:
