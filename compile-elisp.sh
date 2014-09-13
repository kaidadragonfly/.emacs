#!/bin/bash

if [ "$1" ]; then
    emacs -Q -batch -f batch-byte-compile "$1"
else 
    find . -name '*.el' -exec emacs -Q -batch -f batch-byte-compile {} \;
fi
