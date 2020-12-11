#!/usr/bin/env awk -f

BEGIN {RS="\n\n"}

{
  delete g;
  for (i=1; i <= NF; i++)
    for (j=1; j <= length($i); j++) {
      t = substr($i, j, 1)
      if (!(t in g)) {
        count++;
        g[t] = 1
      }
    }
}

END {print count}
