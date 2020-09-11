#!/bin/bash
set -eu -o pipefail
cd "$(dirname "$0")"
set -x
gsi-script -:debug=+ . errno.scm >errno.c
cc -Wall -Wextra -Wno-unused-function -ansi -pedantic -o errno errno.c
./errno | tee errno.out
