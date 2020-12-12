#!/usr/local/bin/awk -f
BEGIN {
    vx = 1
    vy = 0
    x = 0
    y = 0
    ang = 0
}

{
    param = 0 + substr($1, 2, length($1) - 1)
}

/^N/ {
    y += param
}
/^S/ {
    y -= param
}
/^E/ {
    x += param
}
/^W/ {
    x -= param
}
/^F/ {
    x += param * vx
    y += param * vy
}
/^(R|L)/ {
    d = param / 90
    d = d * (substr($1, 1, 1) == "R" ? 1 : -1)
    if (d < 0) d += 4
    
    ang = (ang + d) % 4
    if (ang == 0) {
        vx = 1
        vy = 0
    }
    if (ang == 1) {
        vx = 0
        vy = -1
    }
    if (ang == 2) {
        vx = -1
        vy = 0
    }
    if (ang == 3) {
        vx = 0
        vy = 1
    }
}

#{print $1, x, y, vx, vy}


function abs(z) { return z > 0 ? z : -z}
END {
    print (abs(x) + abs(y))
}