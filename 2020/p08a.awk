#!/usr/bin/env awk -f
{
  ins[NR] = $1
  arg[NR] = $2
}

function exec(    acc, pc) {
  pc = 1
  delete seen
  for(;!seen[pc] && pc <= NR;) {
    seen[pc] = 1
    if (ins[pc] == "acc") acc += arg[pc];
    if (ins[pc] == "jmp") pc += arg[pc]
    else pc += 1
  }
  return pc > NR ? acc : -acc
}

END {
  print -exec() # 2080
}