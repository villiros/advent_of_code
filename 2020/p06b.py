#!/usr/bin/env python2.7
import sys

print sum(len(set.intersection(*[set(p) for p in k.split("\n") if p]))
          for k in open(sys.argv[1]).read().split("\n\n"))
