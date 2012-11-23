.PHONY: travis

travis:
	cabal configure --enable-benchmarks && \
	cabal install --only-dependencies && \
	cabal build && \
	dist/build/perf-data-ringbuffer/perf-data-ringbuffer -N && \
	cabal install
