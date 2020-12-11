#!/usr/local/bin/awk -f
BEGIN {
}
{
    state[NR] = $1
}

function mprint(a,   i) {
    for (i=1; i <= NR; i++)
        print a[i]
    print ""
}

function sat(x, y) {
    if (x in state && y >= 1 && y <= length(state[1]))
        return substr(state[x], y, 1)
}
function mvstate(   i) {
    delete state
    for (i=1; i <= NR; i++) {
        state[i] = state2[i]
    }
}
function cnt(x, y,   o1, o2, res) {
    for (o1 = -1; o1 <= 1; o1++)
        for (o2 = -1; o2 <= 1; o2++)
            if (o1 != 0 || o2 != 0) {
                res += (sat(x + o1, y + o2) == "#") ? 1 : 0
            }
    return res
}
function cntoc(    res) {
    for (i=1; i <= NR; i++)
        for (j = 1; j <= length(state[1]); j++)
            res += sat(i, j) == "#" ? 1 : 0
    return res
}

function doround(   i, j, didchange) {
    delete state2
    for (i=1; i <= NR; i++) {
        for (j=1; j <= length(state[1]); j++) {
            #print sat(i, j), cnt(i, j)
            if (sat(i, j) == "L" && cnt(i, j) == 0) {
                state2[i] = state2[i] "#"
                didchange = 1
            }
            else if (sat(i, j) == "#" && cnt(i, j) >= 4) {
                state2[i] = state2[i] "L"
                didchange = 1
            }
            else
                state2[i] = state2[i] sat(i, j)
        }
    }
    return didchange
}
END {
    #mprint(state)
    doround()
    do {
        #mprint(state2)
        mvstate()
        doround()
    } while (doround())
    #mprint(state)
    print cntoc()
}