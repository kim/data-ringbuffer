module Util
  ( printTiming
  , now
  ) where

import Data.Int
import Data.Time.Clock.POSIX (getPOSIXTime)


now :: IO Double
now = realToFrac `fmap` getPOSIXTime

printTiming :: Int64 -> Double -> Double -> IO ()
printTiming iters start end = do
    let diff = end - start
        tps  = (realToFrac iters) / diff
    putStrLn $ "done in " ++ show diff ++ " sec (" ++ show tps ++ " tps)"


-- vim: set ts=4 sw=4 et:
