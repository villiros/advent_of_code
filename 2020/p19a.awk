#!/usr/bin/env awk -f
function printarr(arr,    p) {
    for (p in arr) print p, ":", arr[p]
}

function remws(s) {
    while (sub(/ /, "", s));
    return s
}

function expand(parts,    pa, pasize, result, i, t) {
    pasize = split(parts, pa, " ")
    
    for (i = 1; i <= pasize; i++) {
        if (pa[i] ~ /^[a-z()|]/) result = result " " pa[i]
        else {
            t = rules[pa[i],1]
            if ((pa[i], 2) in rules) {
                t = "( " t " | " rules[pa[i],2] " )"
            }
            result = result " " t
        }
    }
    
    return result
}

BEGIN {
    FS = ": | [|] "
    state = "rules"
}

NF > 1 {
    for (i=2; i <= NF; i++) {
        if ($i ~ /^"/) $i = substr($i, 2, length($i)-2)
        rules[$1, i-1] = $i
    }
    num_rules++;
}

NF == 0 && state == "rules" {
    printarr(rules)
    
    for (i=1; i < (num_rules); i++) {
        rules[0,1] = expand(rules[0,1])
        print "After", i, rules[0,1]
    }
    state = "msgs"
    msgregex = "^" remws(rules[0,1]) "$"
}

state == "msgs" && match($0, msgregex) {
    print "Matched", $0
    result += 1
}

END {
    print result
}
