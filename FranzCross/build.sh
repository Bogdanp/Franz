#!/usr/bin/env bash

set -euo pipefail

raco exe \
  -o Franz \
  --gui \
  --orig-exe \
  ++aux assets/icon.ico \
  ++aux Franz.desktop \
  ++lang lua \
  ++lib lua/lang/runtime-config \
  ++lib racket/runtime-config \
  ./main.rkt
raco dist dist Franz
