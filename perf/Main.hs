module Main where

import Criterion        (bench)
import Criterion.Config
import Criterion.Main   (defaultMainWith)

import qualified FalseSharing
import qualified TChan.Multicast
import qualified TChan.Unicast
import qualified Disruptor2.Multicast
import qualified Disruptor2.Unicast
import qualified Disruptor2.UnicastBatch


iterations :: Int
iterations = 1000000

config :: Config
config = defaultConfig
    { cfgPerformGC = ljust True
    , cfgSamples   = ljust 5
    }

main :: IO ()
main = defaultMainWith config (return ())
    [ bench "FalseSharing.unpadded"   $ FalseSharing.unpadded iterations
    , bench "FalseSharing.padded"     $ FalseSharing.padded iterations
    , bench "TChan.Multicast"         $ TChan.Multicast.run iterations
    , bench "TChan.Unicast"           $ TChan.Unicast.run iterations
    , bench "Disruptor2.Multicast"    $ Disruptor2.Multicast.run iterations
    , bench "Disruptor2.Unicast"      $ Disruptor2.Unicast.run iterations
    , bench "Disruptor2.UnicastBatch" $ Disruptor2.UnicastBatch.run iterations
    ]
