# Ring Buffer

[![Build Status](https://secure.travis-ci.org/kim/data-ringbuffer.png)](http://travis-ci.org/kim/data-ringbuffer)

Haskell implementation of a concurrent, lock-free, queue-like data structure
(actually a ring buffer), inspired by ["Disruptor"](http://code.google.com/p/disruptor).

## Build

```sh
$ # build the library and tests
$ cabal configure --enable-benchmarks
$ # run the benchmarks
$ GHCRTS='-N' cabal bench
```
