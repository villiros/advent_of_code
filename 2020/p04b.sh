#!/bin/bash

awk '/^$/ {print "NL"} /.+/ {printf "%s ",$0}' $1 |
  awk '/byr:(19[2-9][0-9]|200[0-2])\s/' |
  awk '/iyr:(201[0-9]|2020)\s/' |
  awk '/eyr:(202[0-9]|2030)\s/' |
  awk '/hgt:(((1[5-8][0-9])|(19[0-3]))cm|(59|[6-7][0-9])in)\s/' |
  awk '/hcl:#[0-9a-f]{6}\s/' |
  awk '/ecl:(amb|blu|brn|gry|grn|hzl|oth)\s/' |
  awk '/pid:[0-9]{9}\s/' |
  wc -l |
  xargs

