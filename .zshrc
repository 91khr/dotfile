# Auto generated
HISTFILE=~/.zsh_history
HISTSIZE=500
SAVEHIST=500
bindkey -e
zstyle :compinstall filename '/home/isaac/.zshrc'
setopt correct
autoload -Uz compinit
compinit

# Syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# Enable comments
setopt interactivecomments

# Prompt
setopt PROMPT_SUBST
export PROMPT='%F{3}[%d]%f %(?.%F{2}%?%f.%F{1}%?%f)
%(#.%F{5}%n.%F{6}%n)%f%# '

# Define commands and variables
alias ls="ls --color=auto"
export EDITOR=vim
export PATH=${PATH}:~/bin

# XTerm transparency
[ -n "$XTERM_VERSION" ] && transset-df --id "$WINDOWID" >/dev/null

