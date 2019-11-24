#!/bin/sh

fname=$(basename $1 .silk)
./silk < $1 | llc -o $fname.o -filetype=obj -
