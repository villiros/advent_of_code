#!/usr/bin/env awk -f
BEGIN {
    minx = miny = 1
    maxx = maxy = 1
    minz = maxz = 0
    
    FS = ""
}

{
    for (i=1; i <= NF; i++) {
        if ($i == "#") {
            setactive(actives, i, NR, 0)
        }
    }
}

function doprint(a,    x, y, z) {
    print "====="
    for (z = minz; z <= maxz; z++) {
        print "z=", z
        for (y = miny; y <= maxy; y++) {
            for (x = minx; x <= maxx; x++) {
                printf("%s ", (a[x,y,z] ? "#" : "."))
            }
            print ""
        }
        print ""
        for (y = miny; y <= maxy; y++) {
            for (x = minx; x <= maxx; x++) {
                printf "%s ", count_act_neigh(a, x, y, z)
            }
            print ""
        }
    }
}

function min(a, b) {
    return a < b ? a : b;
}

function max(a, b) {
    return a > b ? a : b;
}

function setactive(destarr, x, y, z) {
    destarr[x, y, z] = 1
    minx = min(x, minx)
    miny = min(y, miny)
    minz = min(z, minz)
    
    maxx = max(x, maxx)
    maxy = max(y, maxy)
    maxz = max(z, maxz)
}

function count_act_neigh(a, x, y, z,    result, xn, yn, zn) {
    result = 0
    for (xn = x-1; xn <= x+1; xn++) {
        for (yn = y-1; yn <= y+1; yn++) {
            for (zn = z-1; zn <= z+1; zn++) {
                if (xn != x || yn != y || zn != z) {
                    if (actives[xn, yn, zn]) result++
                }
            }
        }
    }
    return result;
}

function doround(out,    x, y, z, t) {
    delete out
    for (z = minz - 1; z <= maxz + 1; z++) {
        for (y = miny - 1; y <= maxy + 1; y++) {
            for (x = minx - 1; x <= maxx + 1; x++) {
                t = count_act_neigh(a, x, y, z)
                #printf "%s ", t
                if (actives[x,y,z]) {
                    if (t == 2 || t == 3) {
                        setactive(out, x, y, z)
                    }
                } else {
                    if (t == 3) {
                        setactive(out, x, y, z)
                    }
                }
            }
            #print ""
        }
        #print "z=", z
    }
}

function arrcp(ina, outa,    p) {
    delete outa
    for (p in ina) outa[p] = ina[p]
}

function arrlen(a,    p, result) {
    for (p in a) result++
    return result + 0
}

END {
    #doprint(actives)
    for (i=1; i <= 6; i++) {
        doround(actives2)
        arrcp(actives2, actives)
        #doprint(actives)
    }
    
    print arrlen(actives2)
}