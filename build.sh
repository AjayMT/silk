#!/bin/sh

pushd src
dune build silk.exe --profile release
popd
ln -s _build/default/src/silk.exe silk
