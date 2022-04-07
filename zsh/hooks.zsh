autoload -U add-zsh-hook

# Local directory hook
typeset +x trusted_db=~/.local/share/trusted_dirs.txt forbidden_db=~/.local/share/forbidden_dirs.txt
typeset -A +x trusted_dirs forbidden_dirs
typeset +x prev_local_zshrc
for ln in $([ -r $trusted_db ] && cat $trusted_db); do
    trusted_dirs[$ln]=1
done
for ln in $([ -r $forbidden_db ] && cat $forbidden_db); do
    forbidden_dirs[$ln]=1
done
local function load-local-zshrc() {
    # Unload previous local rc
    [ "$prev_local_zshrc" ] && source "$prev_local_zshrc" -u
    prev_local_zshrc=""
    # Check if the rc should be loaded
    [[ ! -r "$PWD/.zshrc" || "$PWD" == "$(realpath ~)" ]] && return
    [ $forbidden_dirs[$PWD] ] && return
    local answer
    if [ $trusted_dirs[$PWD] ]; then
        answer=yes
    else
        echo -n "Found .zshrc in current directory, trust and execute it? (yes/N[O]/ban): "
        read -r answer
    fi
    # Load the local config
    if [[ "$answer" = "yes" ]]; then
        source "$PWD/.zshrc"
        if [ -z $trusted_dirs[$PWD] ]; then
            trusted_dirs[$PWD]=1
            echo $PWD >>$trusted_db
        fi
        prev_local_zshrc="$PWD/.zshrc"
    elif [[ "$answer" = "ban" && -z $forbidden_dirs[$PWD] ]]; then
        forbidden_dirs[$PWD]=1
        echo $PWD >>$forbidden_db
    fi
}

load-local-zshrc  # Load the rc at startup
add-zsh-hook chpwd load-local-zshrc
