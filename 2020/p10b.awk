#!/usr/local/bin/awk -f
{
    adpts[$1] = 1
}

function trav(nv, num_seen,   i, x)
{
    if (num_seen == NR) return nv + 3
    for (i = nv+1; i <= nv + 3; i++) {
        if (i in adpts && !seen[i]) {
            seen[i] = 1
            x = trav(i, num_seen+1)
            if (x >= 0) return x
            seen[i] = 0
        }
    }
    return -1
}

function trav2(nv, num_seen,   i, x, comb)
{
    #print nv, num_seen, result, device
    if (nv + 3 == device) return 1

    if (nv in memo) return memo[nv]

    for (i = nv+1; i <= nv + 3; i++) {
        if (i in adpts && !seen[i]) {
            seen[i] = 1
            comb += trav2(i, num_seen+1)
            seen[i] = 0
        }
    }
    memo[nv] = comb
    return comb
}

END {
    device = trav(0, 0)
    print "Device", device
    delete seen
    print trav2(0, 0)
}