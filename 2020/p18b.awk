#!/usr/bin/env awk -f
BEGIN {
    stacknext = 1
}

function remws(s) {
    while (sub(/ /, "", s));
    return s
}

function at(s, p) {
    return substr(s, p, 1)
}

function skipnum(s, p) {
    for(; at(s, p) ~ /[0-9]/; p++);
    return p
}

function stpush(v) {
    stack[stacknext++] = v
}

function stpeek() {
    return stack[stacknext-1]
}

function stprint(    i) {
    for (i=1; i < stacknext; i++) {
        printf "%s ", stack[i]
    }
    print ""
}

function stpop() {
    stacknext--
    return stack[stacknext]
}

function do_param(s, p,    t) {
    printf "param     %5s %5s %s x %s\n", p, stpeek(), substr(s, 1, p-1), substr(s, p)
    stprint()
    
    if (at(s, p) == "(") {
        p = do_exp(s, p + 1)
        if (at(s, p) != ")") print "ASSERT 6"
        p++
    } else {
        if (at(s, p) !~ /[0-9]/) print "ASSERT 1", at(s, p)
        t = skipnum(s, p)
        stpush(0 + substr(s, p, (t - p)))
        p = t
    }
    
    printf "param out %5s %5s %s x %s\n", p, stpeek(), substr(s, 1, p-1), substr(s, p)
    stprint()
    
    return p
}

function do_exp(s, p,    t, inside) {
    printf "exp       %5s %5s %s x %s\n", p, stpeek(), substr(s, 1, p-1), substr(s, p)
    stprint()
    
    p = do_param(s, p)
    
    while (p <= length(s) && at(s, p) != ")") {
        t = at(s, p)
        p++
        if (t !~ /[*+]/) print "ASSERT 2"
        
        if (t == "*") {
            p = do_exp(s, p)
            t = stpop() * stpop()
        } else {
            p = do_param(s, p)
            t = stpop() + stpop()
        }
        
        stpush(t)
    }
    
    printf "exp   out %5s %5s %s x %s\n", p, stpeek(), substr(s, 1, p-1), substr(s, p)
    stprint()
    
    return p
}

{
    if (stacknext != 1) print "ASSERT 5"
    do_exp(remws($0), 1)
    result += stpop()
}

END {
    print result
}