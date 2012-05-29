{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Int
import Data.RingBuffer
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO.Unsafe (unsafePerformIO)


size :: Int
size = 1024*8

iterations :: Int64
--iterations = 300000000
iterations = 3000000

main :: IO ()
main = mapM_ run [ ("unicast1P1C", unicast1P1C)
                 , ("diamondPath1P3C", diamondPath1P3C)
                 , ("multicast1P3C", multicast1P3C)
                 , ("pipeline3Step", pipeline3Step)
                 , ("sequencer3P1C", sequencer3P1C) ]

run :: (String, IO ()) -> IO ()
run (s, f) = putStrLn s >> f >> putStrLn ""

unicast1P1C :: IO ()
unicast1P1C = do
  consumer <- newConsumer (return . rnf)
  barrier  <- newProducerBarrier [consumer]
  buf      <- newRingBuffer size 0
  done     <- newEmptyMVar :: IO (MVar Int)
  start    <- now

  forkIO $ produce buf barrier 0 >> putStrLn "producer done"
  forkIO $ consume' buf consumer `finally` putMVar done 1

  takeMVar done >> now >>= printTiming start

diamondPath1P3C :: IO ()
diamondPath1P3C = putStrLn "pending"

multicast1P3C :: IO ()
multicast1P3C = do
  consumers <- mkConsumers 3
  barrier   <- newProducerBarrier consumers
  buf       <- newRingBuffer size 0
  start     <- now

  forkIO $ produce buf barrier 0 >> putStrLn "producer done"
  mapM_ (\consumer -> forkChild $ consume' buf consumer) consumers

  waitForChildren >> now >>= printTiming start

pipeline3Step :: IO ()
pipeline3Step = putStrLn "pending"

sequencer3P1C :: IO ()
sequencer3P1C = putStrLn "pending"

produce :: RingBuffer Int64 -> ProducerBarrier Int64 -> Int64 -> IO ()
produce b barr i =
  unless (i > iterations) $ push b barr i >> produce b barr (i + 1)

consume' :: RingBuffer a -> Consumer a -> IO ()
consume' b c = do
  consume b c
  i <- consumerSequence c
  unless (i >= iterations) $ consume' b c

mkConsumers :: Int -> IO [Consumer Int64]
mkConsumers n = mapM (\_ -> newConsumer (return . rnf)) [1..n]

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
       putMVar children ms
       takeMVar m
       waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar   <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkIO (io `finally` putMVar mvar ())

now :: IO Double
now = realToFrac `fmap` getPOSIXTime

printTiming :: Double -> Double -> IO ()
printTiming start end = do
  let diff = end - start
      tps  = (realToFrac iterations) / diff
  putStrLn $ "done in " ++ show diff ++ " ms (" ++ show tps ++ " tps)"
