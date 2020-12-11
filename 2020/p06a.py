#!/usr/bin/env python2.7
import sys
print sum(len(set.union(*[set(p) for p in k.split("\n")]))
          for k in open(sys.argv[1]).read().split("\n\n"))
