module Util
  ( iterations
  , printTiming
  , now
  ) where

import Data.Time.Clock.POSIX (getPOSIXTime)


iterations :: Int
iterations = 3000000

now :: IO Double
now = realToFrac `fmap` getPOSIXTime

printTiming :: Int -> Double -> Double -> IO ()
printTiming iters start end = do
    let diff = end - start
        tps  = (realToFrac iters) / diff
    putStrLn $ "done in " ++ show diff ++ " sec (" ++ show tps ++ " tps)"


-- vim: set ts=4 sw=4 et:
