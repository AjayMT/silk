#!/bin/sh

pandoc --toc -s -c style.css README.md -o index.html
pandoc --toc -s -c style.css langref.md -o langref.html
