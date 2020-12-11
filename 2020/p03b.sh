#!/bin/bash

dostep() {
  st=$1;
  pos=-$st;
  while read row ; do
    pos=$(( ($pos+$st) % ${#row}))
    echo ${row:$pos:1}
  done | grep '#' | wc -l
}

echo $((`cat $1 | dostep 1` * `cat $1 | dostep 3` * `cat $1 | dostep 5` * `cat $1 | dostep 7` * `awk 'NR % 2 == 1' $1 | dostep 1`))
