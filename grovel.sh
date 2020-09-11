#!/bin/sh
set -eu
cd "$(dirname "$0")"
set -x
gsi-script -:debug=+ . grovel.scm "$@" >groveltmp.c
cc -Wall -Wextra -Wno-unused-function -ansi -pedantic -o groveltmp groveltmp.c
./groveltmp
