#!/usr/bin/env awk -f

function printarr(arr,    p) {
    for (p in arr) print p, ":", arr[p]
}

function remws(s) {
    while (sub(/ /, "", s));
    return s
}

function expand(parts,    pa, pasize, result, i, t, j) {
    pasize = split(parts, pa, " ")
    
    for (i = 1; i <= pasize; i++) {
        if (pa[i] ~ /^[a-z()|+{]/) result = result " " pa[i]
        else {
            t = rules[pa[i],1]
            if (pa[i] != 11) {
                if ((pa[i], 2) in rules) {
                    t = "( " t " | " rules[pa[i],2] " )"
                }
            } else {
                # 11: 42 31 | 42 11 31
                # is the same as 11: 42{N} 31{N}
                # The {xxx} will be left-as is by the regex above
                
                # input strings are max 80 chars, so this should be enough
                for (j=2; j < 50; j++) {
                    t = t " | " "42 {" j "} 31 {" j "}"
                }
                t = "( " t " )"
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
    
    # 8: 42 | 42 8 is same as 8: 42+
    # The expand function leaves "+" field as-is
    if ($1 == 8) {
        rules[8, 1] = "42 +"
    }
    
    num_rules++;
}

NF == 0 && state == "rules" {
    printarr(rules)
    
    while (1) {
        msgregex = expand(rules[0,1])
        print "After", i, rules[0,1]
        if (msgregex == rules[0,1]) break;
        rules[0,1] = msgregex
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