#!/bin/bash

cat $1 |
    perl -npe 's/(\d+)-(\d+) (.): (.+)/$1 $2 $3 $4/' |
    while read l1 l2 ch pw ; do
        pw="_"$pw; c1=${pw:$l1:1}
        c2=${pw:$l2:1}
        [[ $c1 == $ch || $c2 == $ch ]] && [[ $c1 != $c2 ]] && echo YES
    done |
    wc -l |
    # strip whitespace
    xargs
