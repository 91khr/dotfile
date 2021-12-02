#!/usr/bin/bash

find ~/.mutt -type f -name '*.muttrc' -printf "source %p\nfolder-hook \$folder \"source %p\"\n"
test -f ~/.mutt/main.muttrc && echo 'set folder=$my_main_folder'

