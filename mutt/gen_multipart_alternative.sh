#!/bin/sh

pattern='^On .*wrote:$'
path=$(dirname $(readlink -f $0))

echo text/html
echo
pandoc --embed-resources -s -f ${path}/mail_reader.lua -t html --mathml --template ${path}/multipart_tmpl.html --metadata title=" "

