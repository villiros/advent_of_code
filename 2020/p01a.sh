#!/bin/bash

echo test

for i in `cat $1`; do
    t=$((2020-$i))
    grep ^$t'$' $1 >/dev/null && echo $(($i*$t)) && exit 0
done
