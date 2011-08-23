# Ring Buffer

Haskell implementation of a concurrent, lock-free, queue-like data structure
(actually a ring buffer), inspired by ["Disruptor"](http://code.google.com/p/disruptor).

The current implementation is based on mutable vectors in the IO monad. On a
small MacBook Pro (Intel Core 2 Duo @ 2.53 GHz), the test case performs slightly
better than the Java version at around 7.6 million ops/sec and with a very small
memory footprint of 2MB. Unlike the Java version, the performance is very
consistent across runs.

## Installation

Grab the [Haskell Platform](http://hackage.haskell.org/platform)

```sh
$ # for a sandboxed install, get cabal-dev
$ cabal install cabal-dev
$ # build the library and tests
$ cabal-dev install --enable-tests
$ # run the test, setting the value of -N to the number of cores you have
$ time dist/build/perf/perf +RTS -N2
```
