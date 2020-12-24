#!/usr/bin/env awk -f
BEGIN {
}

{
    cups = substr($0, 1, 1)
    for (i=2; i <= length($0); i++)
        cups = cups substr($0, i, 1)
}

function format_cups(s,    i, r) {
    for (i=1; i <= length(s); i++)
        r = r " " substr(s, i, 1)
    return r
}

# Pick 3 cups after pos
function pick_three(pos,    r, t) {
    r = substr(cups, pos + 1, 3)
    if (length(r) == 3) {
        cups = substr(cups, 1, pos) substr(cups, pos + 3 + 1)
    } else {
        t = length(r)
        r = r substr(cups, 1, 3 - length(r))
        cups = substr(cups, 4 - t, (pos - 4 + t + 1))
    }
    return r
}

function find_dest(target,    i, j) {
    for (i=1; i <= 10; i++) {
        for (j=1; j <= length(cups); j++) {
            if (substr(cups, j, 1) == (target - i  < 1 ? target + 9 - i : target - i))
                return j
        }
    }
}

function insert_after(position, to_add) {
    cups = substr(cups, 1, position) to_add substr(cups, position+1)
}

function rotate_r() {
    cups = substr(cups, 2) substr(cups, 1, 1)
}

function do_move(move_num,    taken, destpos) {
    print "-- move", move_num, " ---"
    print "cups:", format_cups(cups)
    taken = pick_three(1)
    print "pick up:", format_cups(taken)
    destpos = find_dest(substr(cups, 1, 1))
    print "destination", destpos
    insert_after(destpos, taken)
    rotate_r()
}

function rotate_1_into_place() {
    while (substr(cups, 1, 1) != 1) {
        rotate_r()
    }
}

function test(    i) {
    for (i=1; i <= 5; i++) {
        cups = 32415
        print cups
        print "pick at", i, pick_three(i), cups
        print "rotate", rotate_r(), cups
    }
    
    for (i=1; i <= 5; i++) {
        cups = "32415"
        print cups
        print "insert at", i, insert_after(i, "xxx"), cups
    }
}

END {
    #test()
    for (i=1; i <= 100; i++) {
        do_move(i)
    }
    
    rotate_1_into_place()
    print substr(cups, 2)
}