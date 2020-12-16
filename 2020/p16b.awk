#!/usr/bin/env awk -f
BEGIN {
    FS = ": | or |-"
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
    for (i=1; i <= NF; i++) {
        my_ticket[i] = $i
    }
    
    for (i=1; i <= NF; i++) {
        for (f in fields) {
            possibles[i, f] = 1
        }
    }
    
    num_fields = NF
    trim_possibles()
}

function trim_possibles(    i, f) {
    for (i=1; i <= num_fields; i++) {
        for (f in fields) {
            if (!((i, f) in possibles))
                continue
            
            if (!((f, $i) in valids))
                delete possibles[i, f]
        }
    }
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
            next;
    }
    
    # Ticket is good
    trim_possibles()
}

END {
    # Resolve ambiguities.
    # Just do it a bunch of times; there won't be more than num_fields ambiguities
    for (iter=1; iter <= num_fields; iter++) {
        for (i=1; i <= num_fields; i++) {
            num_found = 0
            for (f in fields) {
                if ((i, f) in possibles) {
                    num_found++
                    found_field = f
                }
            }
            
            # Remove found_field from all other positions
            if (num_found == 1) {
                for (j=1; j <= num_fields; j++) {
                    if (j != i)
                        delete possibles[j, found_field]
                }
            }
        }
    }
    
    # Assume everything relevant is now resolved.
    result = 1
    for (p in possibles) {
        split(p, p2, SUBSEP)
        if (p2[2] ~ /departure /) {
            result *= my_ticket[p2[1]]
        }
    }
    
    print result
}