#!/usr/bin/env awk -f
BEGIN {
    RS = "\n\n"
    FS = "\n"
}

function get_loop(target,    r, p)
{
    p = 1
    for (r = 1; ; r++) {
         p = (p * 7) % 20201227
         if (p == target)
            return r
    }
}

function do_exp(a, b,    r) {
    r = 1
    while (b > 0) {
        r = (r * a) % 20201227
        b--
    }
    return r
}

{
    loop1 = get_loop($1)
    loop2 = get_loop($2)
    
    print do_exp($2, loop1)
}