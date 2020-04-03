#!/bin/sh

PACKAGE=$(cabal sdist | tail -n 1)
cabal upload "$PACKAGE"
