#!/usr/bin/bash

[ -z "$LOCAL" ] || exit
for f in $(find ~/.mutt -type f -name '*.muttrc'); do
    echo "source $f"
    echo "folder-hook \$folder \"source $f\""
done
test -f ~/.mutt/main.muttrc && echo 'set folder=$my_main_folder'

