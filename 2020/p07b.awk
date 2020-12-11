#!/usr/bin/env awk -f

BEGIN {FS=" bags contain | bags?(, |.)| "} # Adds empty field at the end
#{for (i=1; i <= NF; i++) printf "%s_",$i; print ""}

{
  main = $1" "$2
  for (i=3; index($3, "no") != 1 && i < NF; i+=3) {
    cont[main, $(i+1)" "$(i+2)] = $i
  }
}

function bfind(c) {
  seen[c] = 1
  for (k in cont) {
    split(k, kv, SUBSEP)
    if (kv[2] == c && !(kv[1] in seen)) {
      result++
      bfind(kv[1])
    }
  }
}

function dsearch(c, mult,  acc) {
  for (k in cont) {
    split(k, kv, SUBSEP)
    if (kv[1] == c) {
      acc += cont[k] * mult
      acc += dsearch(kv[2], cont[k]*mult)
    }
  }
  return acc;
}

END {
  bfind("shiny gold")
  print dsearch("shiny gold", 1) # 85324
}