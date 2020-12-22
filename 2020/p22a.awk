#!/usr/bin/env awk -f
BEGIN {
    RS = "\n\n"
    FS = "\n"
}

{
    print NF
    for (i=2; i <= NF; i++) {
        if (NR == 1)
            add_p1($i)
        else
            add_p2($i)
    }
}

function add_p1(card) {
    if (p1)
        p1 = p1 "," card
    else
        p1 = card
}

function add_p2(card) {
    if (p2)
        p2 = p2 "," card
    else
        p2 = card
}

function take_p1(    r, t) {
    if (!p1) print "ASSERT take_p1"
    r = p1 + 0
    t = index(p1, ",")
    if (t) {
        p1 = substr(p1, t+1)
    } else {
        p1 = ""
    }
    return r
}

function take_p2(    r, t) {
    if (!p2) print "ASSERT take_p1"
    r = p2 + 0
    t = index(p2, ",")
    if (t) {
        p2 = substr(p2, t+1)
    } else {
        p2 = ""
    }
    return r
}

function do_round(    p1c, p2c) {
    p1c = take_p1()
    p2c = take_p2()
    
    if (p1c > p2c) {
        add_p1(p1c)
        add_p1(p2c)
    } else {
        add_p2(p2c)
        add_p2(p1c)
    }
}

function get_score(p,    i, r, x, t) {
    t = split(p, x, ",")
    for (i=t; i >= 1; i--)
        r += x[i] * (1 + t - i)
    return r
}

END {
    print "Player 1", p1
    print "Player 2", p2
    
    while (p1 && p2) {
        do_round()
    }
    
    print "Player 1", p1
    print "Player 2", p2
    
    if (p1) {
        print get_score(p1)
    } else {
        print get_score(p2)
    }
}