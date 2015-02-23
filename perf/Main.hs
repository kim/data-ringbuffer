module Main where

import Criterion.Types
import Criterion.Main   (defaultMain)

import qualified FalseSharing
import qualified TChan.Multicast
import qualified TChan.Unicast
import qualified Disruptor2.Multicast
import qualified Disruptor2.Unicast
import qualified Disruptor2.UnicastBatch


iterations :: Int
iterations = 1000000

main :: IO ()
main = defaultMain
    [ bench "FalseSharing.unpadded"   . nfIO . FalseSharing.unpadded       $ iterations
    , bench "FalseSharing.padded"     . nfIO . FalseSharing.padded         $ iterations
    , bench "TChan.Multicast"         . nfIO . TChan.Multicast.run         $ iterations
    , bench "TChan.Unicast"           . nfIO . TChan.Unicast.run           $ iterations
    , bench "Disruptor2.Multicast"    . nfIO . Disruptor2.Multicast.run    $ iterations
    , bench "Disruptor2.Unicast"      . nfIO . Disruptor2.Unicast.run      $ iterations
    , bench "Disruptor2.UnicastBatch" . nfIO . Disruptor2.UnicastBatch.run $ iterations
    ]
