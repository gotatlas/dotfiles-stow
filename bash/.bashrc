#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

# NVM
source /usr/share/nvm/init-nvm.sh

# GO
export PATH="$PATH:$(go env GOPATH)/bin"

# FZF
eval "$(fzf --bash)"
export FZF_DEFAULT_OPTS="--height=40% --layout=reverse --border --preview 'bat --style=numbers --color=always {} | head -500'"

# Doom Emacs
export PATH="$HOME/.emacs.d/bin:$PATH"

# Browser
export BROWSER="brave"

# Aliases
alias cmacs='emacsclient -c -a ""'
alias tmacs='emacsclient -t'
# alias doom-reload='doom sync && pkill -f emacs && sleep 1 && emacs --daemon'

alias nano='micro'
alias ..='cd ..'
alias ...='cd ../..'
alias l='ls -lah'
alias ll='ls -l'
alias la='ls -A'
alias mkdir='mkdir -pv'
alias rm='rm -i'
alias rmrf='rm -Irv'

alias reload='source ~/.bashrc'
alias ebrc='micro ~/.bashrc'
alias reboot='sudo reboot'

alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias venv='python -m venv .venv && source .venv/bin/activate'

# [ -f "/home/atlas/.ghcup/env" ] && . "/home/atlas/.ghcup/env" # ghcup-env

# Created by `pipx` on 2025-08-06 21:41:24
export PATH="$PATH:/home/atlas/.local/bin"
