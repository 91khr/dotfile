#!/bin/sh

function progress() {
    echo -e '\e[33m::\e[0m '"$*"
}
function progexec() {
    progress $*
    eval $*
}

progexec cargo t --all || exit $?
progexec cargo clippy --all || exit $?
progexec cargo fmt --check --all || exit $?

progress find escaping unwraps...
res=1
for f in $(find -name '*.rs'); do
    sed '/#\[cfg(test)\]/q' $f | grep --color=auto --label="$f" -nHF unwrap
    res=$(( $res && $? ))
done
(( $res == 0 )) && exit 1

progress Done.

