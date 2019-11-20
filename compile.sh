#!/bin/sh

fname=$(basename $1 .silk)
./silk < $1 > $fname.llvm
llc -o $fname.s $fname.llvm
as -o $fname.o $fname.s
