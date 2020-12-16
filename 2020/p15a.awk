#!/usr/bin/env awk -f
BEGIN {
    RS = ","
}

function nextn(n, t,    tmp) {
    if (!(n in mem)) {
        mem[n] = t
        return 0
    } else {
        tmp = mem[n]
        mem[n] = t
        return t - tmp
    }
}

{
    lastn = nextn($1, NR)
}

END {
    for (i=NR+1; i < 2020; i++) {
        lastn = nextn(lastn, i)
    }
    print lastn
}
