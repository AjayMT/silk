#!/bin/sh

dune build --profile release
mv -f _build/default/src/silk.exe ./silk
