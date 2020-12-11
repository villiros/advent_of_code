#!/usr/bin/env awk -f
BEGIN {pos=-3}
{
    pos=pos+3
    if (substr($0, 1 + pos % length($0), 1) == "#")
        res += 1
}
END {print res}