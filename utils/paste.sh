#!/bin/bash
cmdbase="curl -F 'c=@-' 'https://fars.ee'"

# Process args
if [[ "image" == "$1"* ]]; then
    cmd="xclip -selection clipboard -o -t image/png | $cmdbase"
elif [[ "text" == "$1"* ]]; then
    cmd="xclip -selection clipboard -o | $cmdbase"
elif [[ "asciinema" == "$1"* ]]; then
    cmd="asciinema rec >(${cmdbase})"
    echo $cmd
elif [[ "file" == "$1"* ]]; then
    cmdbase="curl -F 'c=@${2}' 'https://fars.ee'"
else
    cmd=$cmdbase
fi

# Call it
eval "$cmd"

