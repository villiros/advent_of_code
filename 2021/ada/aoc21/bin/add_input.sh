#!/bin/bash

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
# Changes dir
source $SCRIPT_DIR/common.sh

DAY="p${DAYNUM}"

function usage {
    echo "$0: (input|test) expected?"
    exit 1
}

if [ "$1" != "test" ] && [ "$1" != "input" ]; then
    usage
fi;

if [ "$1" = "test" ] && [ -z "$2" ]; then
    echo "Error: expected must be provided for test";
    usage
fi

if [ "$1" = "input" ] && [ ! -z "$2" ]; then
    echo "Error: expected must not be provided for input"
    usage
fi

if [ "$1" = "test" ]; then
    for i in {0..9}; do
        FNAME="${DAY}_test${i}.txt"
        if [ ! -f "../../input/$FNAME" ]; then
            break
        fi
    done

    if [ -f "../../input/$FNAME" ]; then
        echo "Error: run out of numbers"
        exit 1
    fi

    EXPECTED="$2"
elif [ "$1" = "input" ]; then
    FNAME="${DAY}.txt"
    if [ -f "../../input/$FNAME" ]; then
        echo "ERROR: Input ../../input/$FNAME already exists"
        exit 1
    fi
    EXPECTED='?'
fi

ANSWERLINE="${DAY}a     ${FNAME}     ${EXPECTED}"

echo "# Piping into $FNAME"
cat >"${FNAME}"
echo "# Appending $ANSWERLINE"
(echo "$ANSWERLINE") >> answers