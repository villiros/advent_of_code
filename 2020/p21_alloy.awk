#!/usr/bin/env awk -f

#
# This converts puzzle input into Alloy (https://alloytools.org/) model
# Running it will give a solution, and then one-liners at the bottom massage
# the desired puzzle answers out of it

BEGIN {
    print "abstract sig Ingredient {count: one CountOfUn}"
    print "abstract sig Allergen {contains: one Ingredient}"
    
    FS =" | [(]|, "
    RS = "[)]\n?"
    
}

{
    for (i=1; i <= NF; i++) {
        if ($i != "contains") {
            if (!($i in seen_sigs)) {
                print "one sig", $i, "extends Ingredient {}"
            }
            seen_sigs[$i]++
        } else {
            i++
            break
        }
    }
    
    last_ing = i-2
    
    for (i=last_ing + 2; i <= NF; i++) {
        if (!($i in seen_allergens)) {
            print "one sig", $i, "extends Allergen {}"
        }
        seen_allergens[$i] = 1
    }
    
    printf "\nfact Rule_%s {\n", NR
    for (i = last_ing + 2; i <= NF; i++) {
        printf "    one (%s.contains & (", $i
        for (j=1; j <= last_ing; j++) {
            printf "%s %s ", $j, (j < last_ing ? "+" : "")
        }
        print "))"
    }
    print "}"
}

END {
    # Int isn't particularly usable here, as doing sum(count) blows up SAT space too much.
    # Make each count a unique sig (to account for different ingredients using the same count)
    # Later these can be taken from the model result and massaged into an actual answer.
    print "abstract sig CountOfUn{}"
    for (i in seen_sigs) {
        print "one sig ", ("Count_" i "_" seen_sigs[i]), " extends CountOfUn {}"
    }

    print "\nfact IngCounts {"
    for (i in seen_sigs) {
        printf "    %s.count = %s\n", i, ("Count_" i "_" seen_sigs[i])
    }
    print "}"
    print "fact OneAlergenPerIng {\n    all a, b: Allergen | a != b => no (a.contains & b.contains)\n}"
    print "one sig NoAllerg {\n    contains: set Ingredient,\n    count_appear: set CountOfUn\n}"
    print "fact NoAllergSet {\n    NoAllerg.contains = Ingredient - Allergen.contains"
    print "    NoAllerg.count_appear = NoAllerg.contains.count"
    #print "    NoAllerg.count_appear = (sum x: NoAllerg.contains | x.count)"
    print "}"
    
    # PART 1:
    # Once instance found, copy this/NoAllerg<:count_appear{...} list (...) from the txt output
    # and feed through
    # pbpaste  | awk 'BEGIN{RS=","} {print}' | awk 'BEGIN{FS="_"} {res += $3} END{print res}'
    
    # PART 2:
    # copy the this/Allergen<:contains={...} list from the txt output and feed through:
    # pbpaste  | awk 'BEGIN{RS=", "; FS="->"} {print}' | sort | awk 'BEGIN {FS="->"} {printf "%s,", substr($2, 1, length($2)-2)} END {print ""}'
    # Mind the dangling comma
}
