#!/usr/local/bin/awk -f
BEGIN {
    FS=","
}
NF == 1 {
    starttime = $1
}

NF > 1 {
    for (i = starttime; ; i++) {
        for (j=1; j <= NF; j++) {
            if ($j == "x") continue;
            
            if (i % $j == 0) {
                print $j * (i-starttime)
                exit
            }
        }
    }
}