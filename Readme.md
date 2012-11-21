# Ring Buffer

Haskell implementation of a concurrent, lock-free, queue-like data structure
(actually a ring buffer), inspired by ["Disruptor"](http://code.google.com/p/disruptor).

## Build

```sh
$ # build the library and tests
$ cabal configure --enable-benchmarks
$ # run the benchmarks, setting the value of -N to the number of cores you have
$ dist/build/perf-disruptor2-unicast/perf-disruptor2-unicast +RTS -N2
$ # note that the multicast benchmark requires 4 (physical) cores
$ dist/build/perf-disruptor2-multicast/perf-disruptor2-multicast +RTS -N4
```
