#!/bin/sh

function progress() {
    echo -e '\e[33m::\e[0m '"$*"
}
function progexec() {
    progress $*
    eval $*
}

progexec cargo check --all-features --all-targets || exit $?
progexec cargo test --all-features || exit $?
progexec cargo clippy --all-targets --all-features || exit $?
progexec cargo fmt --check --all || exit $?
progexec cargo doc --all-features || exit $?

progress looking for unbounded unwraps...
res=1
for f in $(find src -name '*.rs'); do
    sed '/#\[cfg(test)\]/q' $f | grep --color=auto --label="$f" -nH '::unwrap\|unwrap()'
    res=$(( $res && $? ))
done
(( $res != 0 )) || exit 1

progress looking for mods without test...
res=1
for f in $(find src -name '*.rs'); do
    if test -z "$(sed -n '${/\/\/\s*Tested/{p; q}}; /#\[cfg(test)\]/{p; q}' $f)"; then
        echo $f
        res=0
    fi
done
(( $res != 0 )) || exit 1

progress Done.

