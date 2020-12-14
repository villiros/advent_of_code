#!/usr/bin/env awk -f
BEGIN {
    mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    FS = " = "
}

function unfloat(v,    vtmp) {
    # sub() changes target!
    vtmp = v
    if (sub(/X/, "0", vtmp)) {
        unfloat(vtmp)
    }
    vtmp = v
    if (sub(/X/, "1", vtmp)) {
        unfloat(vtmp)
    }
    
    if (!sub(/X/, "_", v)) {
        mem[bintodec(v)] = $2
    }
}

function apmask(v,    res, i) {
    for (i=1; i <= length(mask); i++) {
        if (substr(mask, i, 1) != "0") {
            res = res substr(mask, i, 1)
        } else {
            res = res substr(v, i, 1)
        }
    }
    
    unfloat(res)
}

function dectobin(v,    res, i) {
    for (i=0; i < 36; i++) {
        res = (0 + and(v, 1)) res
        v = int(v / 2)
    }
    return res
}

function bintodec(v,    res, i) {
    for (i=1; i <= 36; i++) {
        res *= 2
        res += (substr(v, i, 1) == "1" ? 1 : 0)
        #print v, substr(v, i, 1), res
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
    apmask(dectobin($1))
}

END {
    for (i in mem) res += mem[i]
    print res
}