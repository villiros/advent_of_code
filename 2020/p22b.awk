#!/usr/bin/env awk -f
BEGIN {
    RS = "\n\n"
    FS = "\n"
    
    game_num = 1
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
    p1n++
}

function add_p2(card) {
    if (p2)
        p2 = p2 "," card
    else
        p2 = card
    p2n++
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
    p1n--
    return r
}

function trunc_p1(num,    i, j) {
    p1n = num
    for (i=1; i <= length(p1); i++) {
        if (substr(p1, i, 1) == ",")
            num--
        
        if (num == 0) {
            p1 = substr(p1, 1, i-1)
            return
        }
    }
}

function trunc_p2(num,    i, j) {
    p2n = num
    for (i=1; i <= length(p2); i++) {
        if (substr(p2, i, 1) == ",")
            num--
        
        if (num == 0) {
            p2 = substr(p2, 1, i-1)
            return
        }
    }
}
function count_p1(    r, i) {
    return p1n
}

function count_p2(    r, i) {
    return p2n
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
    p2n--
    return r
}

function do_round(gn, memo,    p1c, p2c, p1s, p2s, p1ns, p2ns, winner) {
    #print "--- Round", " ", "game", gn
    #print "Player 1", p1
    #print "Player 2", p2
    
    
    if ((gn ":" p1 ":" p2) in memo) {
        #print "Player 1 wins in memo"
        p2 = ""
        p2n = 0
        return 1
    }
    
    memo[gn ":" p1 ":" p2] = 1

    p1c = take_p1()
    p2c = take_p2()
    
    #print "Player 1 plays", p1c
    #print "Player 2 plays", p2c
    
    if ((p1c+0) <= count_p1() && (p2c+0) <= count_p2()) {
        #print "Playing a subgame..."
        # Since decks are in global vars, save them here and restore after
        p1s = p1
        p2s = p2
        p1ns = p1n
        p2ns = p2n
        
        trunc_p1(p1c)
        trunc_p2(p2c)
        winner = do_game(game_num++)
        p1 = p1s
        p2 = p2s
        p1n = p1ns
        p2n = p2ns
    } else {
        if (p1c > p2c) winner = 1
        else winner = 2
        
        #print "Winner is", winner
    }
    
    if (winner == 1) {
        add_p1(p1c)
        add_p1(p2c)
    } else {
        add_p2(p2c)
        add_p2(p1c)
    }
}

function do_game(gn,    result, memo) {
    #print "=== Game ", gn
    #print ""
    
    if ((p1 ":" p2) in result_memo)
        return result_memo[p1 ":" p2]
    
    while (p1 && p2) {
        #print "Game", gn, count_p1(), count_p2()
        do_round(gn, memo)
    }
    
    if (p1)
        result = 1
    else
        result = 2
    
    result_memo[p1 ":" p2] = result
    
    return result
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
    
    print do_game(game_num++)
    
    print "Player 1", p1
    print "Player 2", p2
    
    if (p1) {
        print get_score(p1)
    } else {
        print get_score(p2)
    }
}