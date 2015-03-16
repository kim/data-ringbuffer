module Main where

import Criterion.Types
import Criterion.Main   (defaultMain)

import qualified TChan.Multicast
import qualified TChan.Unicast
import qualified Disruptor3.Multicast
import qualified Disruptor3.Unicast
import qualified Disruptor3.Diamond


iterations :: Int
iterations = 1000000

main :: IO ()
main = defaultMain
    [ bench "TChan.Multicast"         . nfIO . TChan.Multicast.run         $ iterations
    , bench "TChan.Unicast"           . nfIO . TChan.Unicast.run           $ iterations
    , bench "Disruptor3.Unicast"      . nfIO . Disruptor3.Unicast.run      $ iterations
    , bench "Disruptor3.Multicast"    . nfIO . Disruptor3.Multicast.run    $ iterations
    , bench "Disruptor3.Diamond"      . nfIO . Disruptor3.Diamond.run      $ iterations
    ]
