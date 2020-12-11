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
  tail -1