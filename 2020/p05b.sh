#!/bin/bash

cat $1 | awk '
  function parse(s, res) {
    s = gensub(/B|R/, "1", "g", s);
    s = gensub(/F|L/, "0", "g", s);
    for(i=1; i<=length(s); i++) {
      res*=2; res+=substr(s,i,1)
    }
    return res;
  }
  {print parse($0) }' |
  sort -n |
  awk '
  BEGIN {prev=9999}
  (cur = ($0+0)) > prev && (cur-prev > 1) {print cur-1}
  {prev = cur}'