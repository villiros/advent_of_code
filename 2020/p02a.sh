#!/bin/bash


cat $1 |
    perl -npe 's/(\d+)-(\d+) (.): (.+)/$1 $2 $3 $4/' |
    while read l1 l2 ch pw ; do
        pw2=`echo $pw | tr -d $ch`
        num=$((${#pw} - ${#pw2}))
        [[ $num -ge $l1 && $num -le $l2 ]] && echo OK
    done |
    wc -l |
    # strip whitespace
    xargs
