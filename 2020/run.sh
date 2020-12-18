#!/bin/bash

shopt -s nullglob
cd "$(dirname "$0")"

ALL_GOOD=1

run() {
    local problem=p$1
    local output="NOT RUN"
    local result
    local expected
    echo == $problem
    
    for part in {a,b}; do
        for inp in input/${problem}*.txt; do
            for sol in ${problem}${part}*.{sh,awk,py}; do
                echo '  ==' $part ./$sol $inp
                
                # expected answer
                expected=$(grep "^${problem}${part}" answers | grep "${inp#input/}" | awk '{print $3}')
                
                if [[ "$expected" == "SKIP" ]]; then
                    output="SKIP"
                else
                    output=$(./$sol $inp)
                    if [[ $? -ne 0 ]]; then
                        output="$output"$'\n'"Non-zero exit code"
                    fi
                fi
                
                # last line
                result=${output##*$'\n'}
                # all but last line
                output=${output%$'\n'*}
                
                if [[ ! -z "$result" && "$expected" != "SKIP" && "$result" == "$expected" ]]; then
                    echo '    ' OK
                elif [[ "$expected" == "SKIP" ]]; then
                    echo '    ' SKIP
                elif [[ -z "$expected" ]]; then
                    echo '    ' NO ANSWER
                else
                    ALL_GOOD=0
                    echo '    ' FAIL
                    echo '    ' Expected: $expected
                    echo '    ' Actual: $result
                    if [[ ! -z "$output" ]]; then
                        echo '    ' Other output:
                        echo "$output"
                    fi
                fi
            done
        done
    done
}

if [[ ! -z $1 ]]; then
    if [[ ${#1} == 1 ]]; then
        run 0${1}
    else
        run $1
    fi
else
    for i in {1..32}; do
        if [[ ${#i} == 1 ]]; then i=0${i}; fi
        if [[ -f input/p${i}.txt ]]; then
            run $i
        fi
    done
fi

if [[ "$ALL_GOOD" == "1" ]]; then
    echo "ALL OK"
else
    echo "THERE WERE ERRORS"
    exit 1
fi