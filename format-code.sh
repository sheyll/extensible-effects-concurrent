#! /usr/bin/env nix-shell
#! nix-shell -i bash

set -ex

find ./src -name '*.hs' -exec ormolu -m inplace -o '-XBangPatterns' -o '-XTypeApplications' '{}' \;
find ./test -name '*.hs' -exec ormolu -m inplace -o '-XBangPatterns' -o '-XTypeApplications' '{}' \;
find ./examples -name '*.hs' -exec ormolu -m inplace -o '-XBangPatterns' -o '-XTypeApplications' '{}' \;

