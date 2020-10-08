# Auto generated
HISTFILE=~/.zsh_history
HISTSIZE=500
SAVEHIST=500
bindkey -e
zstyle :compinstall filename '~/.zshrc'
setopt correct
autoload -Uz compinit
compinit
autoload zed

# Syntax highlighting
HIGHLIGHT_FILE=/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
if test -r $HIGHLIGHT_FILE; then source $HIGHLIGHT_FILE; fi
# Enable comments
setopt interactivecomments
# Pastebin support
function _paste_sh() {
    _arguments -C '1: :(image text asciinema file)'
}
compdef _paste_sh paste.sh

# Prompt
setopt PROMPT_SUBST
export PROMPT='%F{3}[%d]%f %(?.%F{2}%?%f.%F{1}%?%f)
%(#.%F{5}%n.%F{6}%n)%f%# '

# Define commands and variables
alias ls="ls --color=auto"
export EDITOR=vim
export PATH=${PATH}:~/bin

# Utils
if [ $WSL_DISTRO_NAME ]; then
    function start() {
        cmd.exe /c "(start $*)"
        return $?
    }
else
    function start() {
        xdg-open $*
        return $?
    }
fi
function bing() { start "https://bing.com/search?q=${*// /+}" }

