#!/bin/sh

PACKAGE=$(cabal build --enable-documentation --haddock-for-hackage | tail -n 1)
echo "will upload: $PACKAGE"
cabal upload -d "$PACKAGE"
