module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception        (finally)
import Control.Monad            (replicateM, unless)
import Data.Int
import Criterion                (bench)
import Criterion.Main           (defaultMain)
import Util


run :: Int64 -> IO ()
run iterations = do
    chans <- replicateM 3 newTChanIO
    dones <- replicateM 3 newEmptyMVar
    start <- now

    forkIO $ publishTChan chans 0
    mapM_ (\(ch,lck) -> forkChild ch lck) $ zip chans dones

    mapM_ takeMVar dones
    now >>= printTiming iterations start

    where
        publishTChan chans i = unless (i > iterations) $ do
            mapM_ (\chan -> atomically (writeTChan chan i)) chans
            publishTChan chans (i + 1)

        consumeTChan chan i = unless (i > iterations)
            $ atomically (readTChan chan) >> consumeTChan chan (i + 1)

        forkChild chan lck = forkIO $
            consumeTChan chan 0 `finally` putMVar lck ()

main :: IO ()
main = defaultMain [ bench "multicast 3000"    $ run 3000
                   , bench "multicast 30000"   $ run 30000
                   , bench "multicast 3000000" $ run 3000000
                   ]

-- vim: set ts=4 sw=4 et:
