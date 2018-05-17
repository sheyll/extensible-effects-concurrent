#!/usr/bin/env zsh

set -xe

stack build brittany
stack exec brittany -- --write-mode=inplace $( find src -name '*.hs' )
