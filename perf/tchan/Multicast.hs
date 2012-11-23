module TChan.Multicast (run) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception        (finally)
import Control.Monad            (replicateM, unless)
import Util


run :: Int -> IO ()
run i = do
    chans <- replicateM 3 newTChanIO
    dones <- replicateM 3 newEmptyMVar
    start <- now

    forkIO $ publishTChan chans 0
    mapM_ (\(ch,lck) -> forkChild ch lck) $ zip chans dones

    mapM_ takeMVar dones
    now >>= printTiming i start

    where
        publishTChan chans i' = unless (i' > i) $ do
            mapM_ (\chan -> atomically (writeTChan chan i')) chans
            publishTChan chans (i' + 1)

        consumeTChan chan i' = unless (i' > i)
            $ atomically (readTChan chan) >> consumeTChan chan (i' + 1)

        forkChild chan lck = forkIO $
            consumeTChan chan 0 `finally` putMVar lck ()


-- vim: set ts=4 sw=4 et:
