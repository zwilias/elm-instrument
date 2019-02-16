#!/bin/bash

set -ex

PLATFORM="linux-x64"

stack clean

## Build binaries

stack build

mkdir -p dist/package-scripts
ELM_INSTRUMENT="$(stack path --local-install-root)/bin/elm-instrument"
cp "$ELM_INSTRUMENT" dist/package-scripts/elm-instrument
tar zcvf "$PLATFORM".tar.gz -C dist/package-scripts elm-instrument
