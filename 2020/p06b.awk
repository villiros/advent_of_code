#!/usr/bin/env awk -f


BEGIN {RS="\n\n"}

{
  delete g;
  for (i=1; i <= NF; i++)
    for (j=1; j <= length($i); j++)
      g[substr($i, j, 1)]++

  for (k in g)
    if (g[k] == NF) count++;
}

END {print count}
