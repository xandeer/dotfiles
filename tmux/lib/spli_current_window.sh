#!/usr/bin/env bash

TMUX=$(which tmux)
if [ "x${TMUX}" = "x" ]; then
    echo "There is no tmux."
    exit 1
fi

current=$(${TMUX} display-message -p '#W')

if [ "x$1" = "x-v" ]; then
    opt="-v"
elif [ "x$1" = "x-h" ]; then
    opt="-h"
else
    ${TMUX} display "Split window ${current} with $1 failed!"
    exit 1
fi

${TMUX} splitw ${opt} "ssh ${current}"
