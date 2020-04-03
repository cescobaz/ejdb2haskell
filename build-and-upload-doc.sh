#!/bin/sh

PACKAGE=$(cabal haddock --haddock-for-hackage | tail -n 1)
cabal upload -d "$PACKAGE"
