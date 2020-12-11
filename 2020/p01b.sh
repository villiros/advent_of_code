#!/bin/bash

for i in `cat $1`; do
    t=$((2020-$i))
    for j in `cat $1`; do
        t2=$(($t-$j))
        grep ^$t2'$' $1 && echo $(($i*$j*$t2)) && exit 0
    done
done
