#!/usr/bin/env awk -f

BEGIN {
    split("e,se,sw,w,nw,ne", directions, ",")
}

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

function count_around(x, y,    r, d) {
    # Have move modify stuff
    for (d in directions) {
        posx = x; posy = y
        move(directions[d])
        if (tiles[posx, posy]) r++;
    }
    
    return r
}

function do_day(    dest, p, x, y, t, minx, maxx, miny, maxy) {
    for (p in tiles) {
        if (tiles[p]) {
            x = p + 0
            y = substr(p, index(p, SUBSEP) + 1) + 0
            
            minx = x < minx ? x : minx
            maxx = x > maxx ? x : maxx
            miny = y < miny ? y : miny
            maxy = y > maxy ? y : maxy
        }
    }
    
    minx--
    maxx++
    miny--
    maxy++
    
    #print "Bounds", minx, maxx, miny, maxy

    for (x=minx; x <= maxx; x++)
        for (y=miny; y <= maxy; y++) {
            if (tiles[x,y]) {
                // BLACK
                t = count_around(x, y)
                if (!(t == 0 || t > 2))
                    dest[x,y] = 1
            } else {
                // WHITE
                t = count_around(x, y)
                if (t == 2)
                    dest[x,y] = 1
            }
        }
    
    delete tiles
    for (p in dest)
        tiles[p] = dest[p]
}

function count_black(    p, r) {
    for (p in tiles)
        if (tiles[p])
            r++;
    
    return r
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
    for (i=1; i <= 100; i++) {
        do_day()
        print "Day", i, count_black()
    }
    
    print count_black()
}