#!/usr/bin/env bash

set -euo pipefail

raco exe \
  -o Franz \
  --gui \
  --orig-exe \
  ++aux assets/icon.ico \
  ++aux flatpak/io.defn.Franz.desktop \
  ++lang lua \
  ++lib lua/lang/runtime-config \
  ++lib racket/runtime-config \
  ./main.rkt
raco dist dist Franz
cp assets/icon.svg dist/
cp flatpak/io.defn.Franz.desktop dist/
cp flatpak/io.defn.Franz.appdata.xml dist/
tar cvzf \
    Franz.tar.gz \
    -C dist \
    bin lib icon.svg \
    io.defn.Franz.appdata.xml \
    io.defn.Franz.desktop
