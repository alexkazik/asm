#!/bin/sh

pushd asm-data
stack build --force-dirty --fast --pedantic --ghc-options="-fforce-recomp"
popd

pushd asm
stack build --force-dirty --fast --pedantic --ghc-options="-fforce-recomp"
popd

pushd asm-cpu6502
stack build --force-dirty --fast --pedantic --ghc-options="-fforce-recomp"
popd

pushd demo
stack build --force-dirty --fast --pedantic --ghc-options="-fforce-recomp"
popd
