#!/usr/bin/env bash

set -e

HERE=$(realpath $(dirname "${0}"))
pushd "${HERE}"

# cabal haddock --haddock-hoogle --haddock-all --haddock-internal  --haddock-hyperlink-source    --haddock-quickjump
cabal haddock  --haddock-all --haddock-hyperlink-source    --haddock-quickjump

popd
