#!/bin/bash

pos=-3
cat $1 |
    while read row ; do
        pos=$(( ($pos+3) % ${#row}))
        echo   ${row:$pos:1}
    done |
    grep '#' |
    wc -l |
    # Strip whitespace
    xargs
