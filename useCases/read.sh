CMD="node bin/read.js"
ARGS=""

while getopts "hi:" flag
do
    case "${flag}" in
        h) ARGS="$ARGS -h";;
        i) ARGS="$ARGS -i ${OPTARG}";;
    esac
done

# echo "Command: $CMD $ARGS"
$CMD $ARGS