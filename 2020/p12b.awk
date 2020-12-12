#!/usr/local/bin/awk -f
BEGIN {
    x = 10
    y = 1
    ang = 0
    
    shx = 0
    shy = 0
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
    shx += x * param
    shy += y * param
}


/^(R|L)/ {
    d = param / 90
    d = d * (substr($1, 1, 1) == "R" ? 1 : -1)
    if (d < 0) d += 4
    ox = x
    oy = y

    if (d == 1) {
        x = oy
        y = -ox
    }
    if (d == 2) {
        x = -ox
        y = -oy
    }
    if (d == 3) {
        x = -oy
        y = ox
    }
}

#{print $1, shx, shy, ";", x, y, ";", vx, vy}


function abs(z) { return z > 0 ? z : -z}

END {
    print (abs(shx) + abs(shy))
}