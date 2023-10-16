#!/usr/bin/env bash

set -euo pipefail

raco exe \
  -o Franz \
  --gui \
  --ico ./assets/icon.ico \
  ++lang lua \
  ++lib lua/lang/runtime-config \
  ++lib racket/runtime-config \
  ./main.rkt
raco dist dist Franz
