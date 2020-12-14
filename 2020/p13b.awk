#!/usr/local/bin/awk -f
BEGIN {
    FS=","
}
NF == 1 {
    starttime = $1
}

NF > 1 {
    stride = $1
    # Timestamp so far
    t = 0
    # Time delta to bus in $nextn
    dt = 1
    for (nextn=2; nextn <= NF; nextn++) {
        for (; $nextn == "x"; nextn++) {
            dt++;
        }
        
        for (i=0; (t + stride * i + dt) % $nextn != 0; i++);
        
        t = t + stride * i
        
        # Assumes all bus IDs are coprime
        stride *= $nextn
        dt += 1
    }
    print t
}