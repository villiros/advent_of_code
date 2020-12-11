#!/bin/bash

awk '/^$/ {print "NL"} /.+/ {printf "%s ",$0}' $1 | grep byr: | grep iyr: | grep eyr: | grep hgt: | grep hcl: | grep ecl: | grep pid: | wc -l | 
# Strip whitespace
xargs
