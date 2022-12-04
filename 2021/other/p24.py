#!/usr/bin/env python3
import sys

# pip install z3-solver
from z3 import *

PRINTDEBUG = False

def read_input():
    result = []
    for i in sys.stdin:
        i = i.rstrip()
        
        if i.startswith("inp"):
            result.append(i.split(' '))
        else:
            op, left, right = i.split(' ')
            try:
                left = int(left)
            except ValueError:
                pass
            try:
                right = int(right)
            except ValueError:
                pass
            
            result.append((op, left, right))
    
    return result

# Symbolic execution of a program
# Concrete values get propagated as appropriate too.
# Also does trivial optimizations (such as mul x 0 == 0)
def symb_exec(prog, inputs, debug=False):
    inputs = list(inputs)
    state = {'w': 0, 'x': 0, 'y': 0, 'z': 0}
    
    # This works because z3 overrides operators on
    # expressions (symbolic). But concrete values will
    # just get normal python treatment and produce concrete results
    def eadd(left, right):
        # x + 0 = x
        if right == 0:
            return left
        # 0 + x = x
        if left == 0:
            return right
        return left + right
    
    def emul(left, right):
        # x * 0 = 0
        # x * 1 = x
        if right == 0 or left == 0:
            return 0
        if right == 1:
            return left
        if left == 1:
            return right
        return left * right
    
    def ediv(left, right):
        # x / 1 = x
        if right == 1:
            return left

        # z3 does not understand //, but want to do it for concrete
        if isinstance(left, int) and isinstance(right, int):
            return left // right
        
        return left / right
    
    def emod(left, right):
        return left % right
    
    def eeql(left, right):
        if isinstance(left, int) and isinstance(right, int):
            return 1 if left == right else 0
        
        # Need to sign-extend values here to make overall expressions
        # match our 64-bit machine
        return z3.If(left == right, BitVecVal(1, 64), BitVecVal(0, 64))
    
    disp = {'add': eadd, 'mul': emul, 'div': ediv, 'mod': emod, 'eql': eeql}
    
    for inst in prog:
        if debug:
            print(state)
            
        if inst[0] == 'inp':
            state[inst[1]] = inputs.pop(0)
        else:
            left = state[inst[1]] if not isinstance(inst[1], int) else inst[1]
            right = state[inst[2]] if not isinstance(inst[2], int) else inst[2]
            state[inst[1]] = disp[inst[0]](left, right)
    
    # All inputs consumed
    assert not inputs
    
    return state
    
def main(prog, shouldMaximize = True):
    s = Optimize()
    
    # Inputs
    serial = [BitVec('digit_{}'.format(i), 64) for i in range(14, 0, -1)]
    
    # All serial digits are 1..9
    for i in serial:
        s.add(i > 0, i < 10)
    
    final_state = symb_exec(prog, serial)
    
    # We are only interested in z == 0
    s.add(simplify(final_state['z']) == 0)
    
    if PRINTDEBUG:
        print("Z:")
        print(final_state['z'].sexpr())
    
    # Expression for the serial as a number. Will need to be Sum()
    opt_expr = []
    t = 1
    for i in serial:
        opt_expr.append(i * BitVecVal(t, 64))
        t *= 10
    
    # Part A/B
    if shouldMaximize:
        s.maximize(Sum(opt_expr))
    else:
        s.minimize(Sum(opt_expr))
    
    if not s.check():
        raise Exception("Unsat %s" % (s,))
    
    result = 0
    model = s.model()
    for i in serial:
        result = result * 10 + model[i].as_long()
    
    #
    # Check: concrete execution with the result to double-check it is valid
    #
    conc_serial = []
    t = result
    for i in range(14):
        conc_serial.append(t % 10)
        t = t // 10
    
    conc_serial.reverse()
    
    assert (symb_exec(prog, conc_serial)['z']) == 0
    
    return result

if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == '-d':
        PRINTDEBUG = True
    
    prog = read_input()
    print("PartA:", main(prog, True))
    print("PartB:", main(prog, False))