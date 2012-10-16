#!/bin/bash

find . -name '*.el' -exec emacs -batch -f batch-byte-compile {} \;
