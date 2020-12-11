#!/usr/bin/env awk -f
BEGIN {RS="\n\n"}
/byr:/ && /iyr:/ && /eyr:/ && /hgt:/ && /hcl:/ && /ecl:/ && /pid:/ {num+=1}
END {print num}