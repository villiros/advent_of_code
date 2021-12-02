SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

cd $SCRIPT_DIR/..

DAYNUM=$(ls solutions/p**.ads | awk -F/ '{printf("%02i\n", substr($2, 2, 2))}' | sort -n | tail -n1)
NEXTDAYNUM=$(ls solutions/p**.ads | awk -F/ '{printf("%02i\n", substr($2, 2, 2) + 1)}' | sort -n | tail -n1)