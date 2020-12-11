#!/bin/bash

awk 'BEGIN {pos=-3} {pos=pos+3; print substr($0, 1 + pos % length($0), 1)}' $1 | grep "#" | wc -l | xargs
