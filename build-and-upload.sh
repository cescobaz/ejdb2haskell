#!/bin/sh

cabal build --enable-documentation
cabal test

PACKAGE=$(cabal sdist | tail -n 1)
cabal upload "$PACKAGE"
