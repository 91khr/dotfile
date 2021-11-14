#!/bin/sh

pattern='^On .*wrote:$'

echo text/html
echo
sed -n -e "/$pattern/{x;p;x}; /$pattern/,\${p;b}" \
    -e '/./{H;$!d};x; s/ \+\n> \+/ /g; s/ \+\n/ /g; p' | \
    pandoc -s -f gfm+hard_line_breaks -t html --template multipart_tmpl.html --metadata title=" "

