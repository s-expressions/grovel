#!/bin/sh
set -eu
cd "$(dirname "$0")"
file="$1"
shift
set -x
gsi-script -:debug=+ . grovel.scm "$file" >groveltmp.c
cc -Wall -Wextra -Wno-unused-function -ansi -pedantic -o groveltmp groveltmp.c "$@"
./groveltmp
