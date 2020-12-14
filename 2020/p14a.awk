#!/usr/bin/env awk -f
BEGIN {
    mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    FS = " = "
}

function apmask(v,    res, i) {
    for (i=1; i <= length(mask); i++) {
        if (substr(mask, i, 1) != "X") {
            res = res substr(mask, i, 1)
        } else {
            res = res substr(v, i, 1)
        }
    }
    return res
}

function dectobin(v,    res, i) {
    for (i=0; i < 36; i++) {
        res = (0 + v % 2) res
        v = int(v / 2)
    }
    return res
}

function bintodec(v,    res, i) {
    for (i=1; i <= 36; i++) {
        res *= 2
        res += substr(v, i, 1)
    }
    return res
}

/^mem/ {
    # mem[XXX]
    $1 = substr($1, 5, length($1) - 5)
}

$1 == "mask" {
    mask = $2
}

$1 != "mask" {
    mem[$1] = bintodec(apmask(dectobin($2)))
}

END {
    for (i in mem) res += mem[i]
    print res
}