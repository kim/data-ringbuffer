.PHONY: travis

travis:
	cabal configure --enable-benchmarks && \
	cabal build && \
	dist/build/perf-data-ringbuffer/perf-data-ringbuffer -N  && \
	cabal install
