#!/bin/bash

cat $1 |
    while IFS=" -:" read l1 l2 ch pw ; do
        pw2=${pw//$ch/};
        num=$((${#pw} - ${#pw2})) ;
        [[ $num -ge $l1 && $num -le $l2 ]] && echo OK ;
    done |
    wc -l |
    # strip whitespace
    xargs
