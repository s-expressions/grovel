#!/bin/bash
set -eu -o pipefail
cd "$(dirname "$0")"
set -x
gsi-script -:debug=+ . example.scm >example.c
cc -Wall -Wextra -Wno-unused-function -ansi -pedantic -o example example.c
./example | tee example.out
