#!/bin/bash

cat $1 |
    while IFS=" -:" read l1 l2 ch pw ; do
        pw="_"$pw
        c1=${pw:$l1:1}
        c2=${pw:$l2:1}
        [[ $c1 == $ch || $c2 == $ch ]] && [[ $c1 != $c2 ]] && echo YES
    done |
    wc -l |
    # strip whitespace
    xargs
