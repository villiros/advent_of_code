#!/usr/bin/env awk -f

{ins[NR] = $1
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
  return pc > NR ? acc : -1
}

function partb(    ci, t) {
  for (ci=1; ci <= NR; ci++) {
    if (ins[ci] == "jmp") {
      ins[ci] = "nop"
      t = exec()
      if (t > 0) return t
      ins[ci] = "jmp"
    } else if (ins[ci] == "nop") {
      ins[ci] = "jmp"
      t = exec()
      if (t > 0) return t
      ins[ci] = "nop"
    }
  }
}

END {
  print partb() # 2477
}