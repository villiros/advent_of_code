#!/usr/bin/env python3
import sys
from collections import defaultdict

# pip install z3-solver
from z3 import *

def read_input():
    result = []
    for l in sys.stdin:
        obvs, unkn = l.rstrip('\n').split('|')
        obvs = [i for i in obvs.rstrip().split(" ")]
        unkn = [i for i in unkn.lstrip().split(" ")]

        result.append((obvs, unkn))
    
    return result

SEGMENTS = "abcdefg"

DIGITS = {
    0: "abcefg",
    1: "cf",
    2: "acdeg",
    3: "acdfg",
    4: "bcdf",
    5: "abdfg",
    6: "abdefg",
    7: "acf",
    8: "abcdefg",
    9: "abcdfg"
}

def solve_input_line(obvs, unkn):
    s = Solver()

    # Seen_<i>_<seg> \in {1, 0}: 1 if seg is in input
    # Includes unknown (inputs to resolve), they need to satisfy the constraints too
    inputs = defaultdict(dict)    
    for i, o in enumerate(obvs + unkn):
        for seg in SEGMENTS:
            name = "Seen_{}_{}".format(i, seg)
            v = Int(name)
            inputs[i][seg] = v
            if seg in o:
                s.add(v == 1)
            else:
                s.add(v == 0)
    
    # We'll need to resolve unkn at the end and need their indexes.
    # Thankfully there's always only 4 of them
    assert len(unkn) == 4
    unkn_indexes = list(range(len(obvs), len(inputs)))
    
    # Connections from segA to segB:
    # Conn_<segA>_<segB> \in {1, 0}
    # Connections are biderictional, but the connection constraint below implies that.
    connections = defaultdict(dict)
    for segA in SEGMENTS:
        for segB in SEGMENTS:
            name = "Conn_{}_{}".format(segA, segB)
            v = Int(name)
            connections[segA][segB] = v
            s.add(Or((v == 1), (v == 0)))
    
    # Connection constraints
    # \A s \in SEGMENTS: sum(Conn_<s>_<*>) = 1 /\ sum(Conn_*_<s>) = 1
    for seg in SEGMENTS:
        s.add(Sum([connections[seg][other] for other in SEGMENTS]) == 1)
        s.add(Sum([connections[other][seg] for other in SEGMENTS]) == 1)
    
    # Outputs. Need for inputs and unknowns for constraint purposes
    # Out_<i>_<seg> \in {1, 0}: 1 if segment is on
    outputs = defaultdict(dict)
    for i in inputs.keys():
        for seg in SEGMENTS:
            name = "Out_{}_{}".format(i, seg)
            v = Int(name)
            outputs[i][seg] = v
            s.add(Or(v == 1, v == 0))

    # Now wire up inputs to outputs using connections
    # For a single input i to output segment sOut:
    #   /\ Out_i_<sOut> = sum(\A s \in SEGMENTS: Seen_i_<s> * Conn_<s>_<sOut>)
    #   /\ Out_i_<sOut> \in {0, 1}  \* Already defined when defining output variables
    for i in inputs.keys():
        for sOut in SEGMENTS:
            s.add(outputs[i][sOut] == Sum([inputs[i][s] * connections[s][sOut] for s in SEGMENTS]))
    
    # Finally, constrain outputs to be valid digits
    # For every output i:
    # \E dig \in DOMAIN DIGITS: \A seg \in SEGMENTS: Out_i_<seg> = IF seg \in DIGITS[dig] THEN 1 ELSE 0
    # Quantifiers are a bit of a pain in solvers, so here it's unrolled
    for i in inputs.keys():
        s.add(
            Or([
                And([
                        outputs[i][seg] == (1 if seg in DIGITS[digit] else 0)
                        for seg in SEGMENTS
                    ])
                for digit in DIGITS.keys()]))

    if not s.check():
        raise Exception("Unsat %s" % (s,))
    else:
        model = s.model()
        result = 0
        for i in unkn_indexes:
            r = ""
            for seg in SEGMENTS:
                if model[outputs[i][seg]].as_long():
                    r += seg

            # Already sorted
            for k, v in DIGITS.items():
                if v == r:
                    result = result * 10 + k
                    break
        
    
    return result


def main():
    input = read_input()

    result = 0
    for obvs, unkn in input:
        # Outputs the number of that line
        result += solve_input_line(obvs, unkn)
    
    print("OUT: ", result)


if __name__ == '__main__':
    main()