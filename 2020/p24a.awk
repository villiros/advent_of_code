#!/usr/bin/env awk -f

function move(dir) {
    #print "move", dir, posx, posy
    if (dir == "e") {
        posx +=2
    }
    if (dir == "se") {
        posx += 1
        posy -= 1
    }
    if (dir == "sw") {
        posx -= 1
        posy -= 1
    }
    if (dir == "w") {
        posx -= 2
    }
    if (dir == "nw") {
        posx -= 1
        posy += 1
    }
    if (dir == "ne") {
        posx += 1
        posy += 1
    }
    #print "    ", dir, posx, posy
}

{
    posx = posy = 0
    
    for (i=1; i <= length($0); i++) {
        t = substr($0, i, 1)
        if (t == "s" || t == "n") {
            move(substr($0, i, 2))
            i++
        } else {
            move(substr($0, i, 1))
        }
    }
    
    tiles[posx,posy] = !tiles[posx,posy]
}

END {
    for (p in tiles) {
        if (tiles[p])
            result++
    }
    
    print result
}