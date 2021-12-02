#!/bin/bash

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

cd $SCRIPT_DIR/..

DAYNUM=$(ls solutions/p**.ads | awk -F/ '{printf("%02i\n", substr($2, 2, 2) + 1)}' | sort -n | tail -n1)
DAY="p${DAYNUM}"

read -p "Adding new day $DAY. Press ENTER to continue"

if ! git diff --exit-code solutions/solutions.adb ; then
    echo ERROR: File solutions/solutions.adb is modified in git
    exit 1
fi;

echo "## Adding solutions"
cat solutions/p00.adb | awk -v DAY="${DAY}" '{gsub(/p00/, DAY, $0); print}' > solutions/$DAY.adb
cat solutions/p00.ads | awk -v DAY="${DAY}" '{gsub(/p00/, DAY, $0); print}' > solutions/$DAY.ads

echo "## Adding to the dispatcher"
cat solutions/solutions.adb | awk -v DAY="${DAY}" '
/ADVENT_ADD_WITH/ {print "with " DAY ";"}
/ADVENT_ADD_GET_DISPATCHERS/ {print "        " DAY ".GetDispatchers(D);"}
{print}' > /tmp/advent_tmp
cat /tmp/advent_tmp > solutions/solutions.adb

echo "
# 
# Day ${DAYNUM}
#" >> answers

echo "## DONE"

