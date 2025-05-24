#!/usr/bin/env bash

set -euo pipefail

raco exe \
  -o Franz \
  --gui \
  --orig-exe \
  ++aux assets/icon.ico \
  ++lang lua \
  ++lib lua/lang/runtime-config \
  ++lib racket/runtime-config \
  ./main.rkt
raco dist dist Franz
cp assets/icon.svg dist/
tar cvzf \
    Franz.tar.gz \
    -C dist \
    bin lib icon.svg
