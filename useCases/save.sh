CMD="node bin/save"
ARGS=""

while getopts "ho:" flag
do
    case "${flag}" in
        h) ARGS="$ARGS -h";;
        i) ARGS="$ARGS -o ${OPTARG}";;
    esac
done

# echo "Command: $CMD $ARGS"
$CMD $ARGS