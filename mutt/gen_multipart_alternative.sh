#!/bin/sh

pattern='^On .*wrote:$'
path=$(dirname $(readlink -f $0))

echo text/html
echo
sed -n -e "/$pattern/{x;p;x}; /$pattern/,\${p;b}" \
    -e '/./{H;$!d};x; s/ \+\n> \+/ /g; s/ \+\n/ /g; p' | \
    pandoc -s -f gfm+hard_line_breaks -t html --template ${path}/multipart_tmpl.html --metadata title=" "

