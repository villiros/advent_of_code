#!/usr/bin/env awk -f
BEGIN {
  RS="\n\n"
  FS="[ \n]"
  num = 0
}

{
  delete fields;
  for(i=1; i <= NF; i++) {
    split($i, kv, ":")
    fields[kv[1]] = kv[2]
  }
}

(fields["byr"] >= 1920 && fields["byr"] <= 2002) &&
(fields["iyr"] >= 2010 && fields["iyr"] <= 2020) &&
(fields["eyr"] >= 2020 && fields["eyr"] <= 2030) &&
(((fields["hgt"] ~ /cm/) && (fields["hgt"]+0) >= 150 && (fields["hgt"]+0) <= 193) ||
 ((fields["hgt"] ~ /in/) && (fields["hgt"]+0) >= 59  && (fields["hgt"]+0) <= 76)) &&
fields["hcl"] ~ /^#[0-9a-f]{6}$/ &&
fields["ecl"] ~ /^(amb|blu|brn|gry|grn|hzl|oth)$/ &&
fields["pid"] ~ /^[0-9]{9}$/ {
  num += 1
}

END {print num}