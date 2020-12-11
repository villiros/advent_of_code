#!/usr/local/bin/awk -f
{
    adpts[$1] = 1
}

function trav(nv, num_seen, num1, num3,  i, x)
{
    #print nv, num_seen, num1, num3
    if (num_seen == NR) return num1*num3
    for (i = nv+1; i <= nv + 3; i++) {
        if (i in adpts && !seen[i]) {
            seen[i] = 1
            x = trav(i, num_seen+1, num1 + (i == nv + 1 ? 1 : 0), num3 + (i == nv + 3 ? 1 : 0))
            if (x >= 0) return x
            seen[i] = 0
        }
    }
    return -1
}

END {
    print trav(0, 0, 0, 1)
}