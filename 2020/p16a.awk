#!/usr/bin/env awk -f
BEGIN {
    FS = "(: )|( or )|-"
}

/ or / && NF == 5 {
    fields[$1] = 1
    
    for (i=$2; i <= $3; i++) valids[$1,i] = 1;
    for (i=$4; i <= $5; i++) valids[$1,i] = 1;
}

/your ticket:/ {
    FS = ","
    
    tick_type = "your"
}

NF > 2 && tick_type == "your" {
    # my ticket
}

/nearby tickets:/ {
    tick_type = "nearby"
}

NF > 2 && tick_type == "nearby" {
    for (i=1; i <= NF; i++) {
        found = 0
        for (f in fields) {
            if ((f, $i) in valids)
                found = 1
        }
        
        if (!found)
            result += 0 + $i
    }
}

END {
    print result
}