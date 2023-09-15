# Auto generated
HISTFILE=~/.zsh_history
HISTSIZE=500
SAVEHIST=2000
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS
bindkey -e

zstyle ':completion:*' completer _complete _ignored _correct _approximate
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'r:|[._-]=** r:|=**'
zstyle ':completion:*' max-errors 2 numeric
zstyle ':completion:*' menu select=4
zstyle ':completion:*' prompt '(%e typo)'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' squeeze-slashes true
zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit
autoload zed

# Syntax highlighting
HIGHLIGHT_FILE=/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
test -r "$HIGHLIGHT_FILE" && source "$HIGHLIGHT_FILE"
unset HIGHLIGHT_FILE
# Enable comments
setopt interactivecomments

# Prompt
setopt PROMPT_SUBST
if [ -n "$TMUX" -o "$VIM" ]; then
    [ $SHLVL -gt 2 ] && LVLPROMPT="%F{5}(lv $SHLVL)%f "
else
    [ $SHLVL -gt 1 ] && LVLPROMPT="%F{5}(lv $SHLVL)%f "
fi
export PROMPT="%F{3}[$([ -n "$SSH_CLIENT" -o -n "$SSH_TTY" ] && echo $HOST:)%~]\
%f %(?.%F{2}%?.%F{1}%?)%f ${LVLPROMPT}%(1j.%F{5}(%j job%(2j.s.))%f.)
%(#.%F{5}%n.%F{6}%n)%f%# "
unset LVLPROMPT

# Define commands and variables
alias ls="ls --color=auto"
alias grep="grep --color=auto --line-number"
export EDITOR=vim
export PATH="${PATH}"
export GPG_TTY="$(tty)"
