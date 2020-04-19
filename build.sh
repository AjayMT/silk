#!/bin/sh

pandoc --highlight-style=kate --toc -s -c style.css README.md -o index.html
pandoc --highlight-style=kate --toc -s -c style.css syntax.md -o syntax.html
