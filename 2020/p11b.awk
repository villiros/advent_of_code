#!/usr/local/bin/awk -f
BEGIN {
}
{
    state[NR] = $1
    for (x=-1; x <= 1; x++)
        for (y=-1; y <= 1; y++)
            if (x != 0 || y != 0) dirs[x,y] = 1
}

function mprint(a,   i) {
    for (i=1; i <= NR; i++)
        print a[i]
    print ""
}

function sat(x, y) {
    if (x in state && y >= 1 && y <= length(state[1]))
        return substr(state[x], y, 1)
    return " "
}
function mvstate(   i) {
    delete state
    for (i=1; i <= NR; i++) {
        state[i] = state2[i]
    }
}
function cnt(x, y,   oo, kv, o1, o2, m, t, res) {
    for (oo in dirs) {
        split(oo, kv, SUBSEP)
        o1 = kv[1]
        o2 = kv[2]
        for (m=1; ; m++) {
            t = sat(x + o1*m, y + o2*m)
            res += t == "#" ? 1 : 0
            if (t != ".") break;
        }
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
            else if (sat(i, j) == "#" && cnt(i, j) >= 5) {
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