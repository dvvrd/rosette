#!/bin/bash

# Usage: simply run.sh to run default benchmarks (~/rosette/test/unbound)
# or BM=<path/to/benchmarks> ./run.sh

BM=${BM:-~/rosette/test/unbound}

echo "Using benchmarks from $BM"

for f in $BM/**/*.rkt ; do
  killall -q spacer
  killall -q racket
  echo "Running $f..."
  racket $f
done;

echo "DONE!"
