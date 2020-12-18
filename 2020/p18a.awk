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

function stpop() {
    stacknext--
    return stack[stacknext]
}

function do_param(s, p,    t) {
    print "param", p, substr(s, p)
    
    if (at(s, p) == "(") {
        p = do_exp(s, p + 1)
        if (at(s, p) != ")") print "ASSERT PAX"
        p++
    } else {
        if (at(s, p) !~ /[0-9]/) print "ASSERT 1", at(s, p)
        t = skipnum(s, p)
        stpush(0 + substr(s, p, (t - p)))
        p = t
    }
    
    return p
}

function do_exp(s, p,    t) {
    p = do_param(s, p)
    
    while (p <= length(s) && at(s, p) != ")") {
        t = at(s, p)
        p++
        if (t !~ /[*+]/) print "ASSERT 2"
        
        p = do_param(s, p)
        
        if (t == "*")
            t = stpop() * stpop()
        else if (t == "+")
            t = stpop() + stpop()
        
        stpush(t)
    }
    
    return p
}

{
    do_exp(remws($0), 1)
    result += stpop()
}

END {
    print result
}