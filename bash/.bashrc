#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

#export GTK_THEME=Dracula

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

# Editor
export EDITOR="doom-now"

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

alias untar='tar -xzvf'

alias reload='source ~/.bashrc'
alias ebrc='micro ~/.bashrc'
alias reboot='sudo reboot'

alias rapps='update-desktop-database ~/.local/share/applications'

alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias venv='python -m venv .venv && source .venv/bin/activate'

# ~/.bashrc or ~/.zshrc

# grope: cd to the dir of the first file whose contents match
grope() {
  local pat="$*"
  local f
  # GNU grep: NUL-separated to survive spaces/newlines
  IFS= read -r -d '' f < <(grep -rIlZ -- "$pat" .)
  [ -n "$f" ] && cd -- "$(dirname -- "$f")" || { echo "grope: no match" >&2; return 1; }
}

# gropen: open the first matching file in your running Emacs (or start it)
gropen() {
  local pat="$*"
  local f
  IFS= read -r -d '' f < <(grep -rIlZ -- "$pat" .)
  [ -z "$f" ] && { echo "gropen: no match" >&2; return 1; }
  if emacsclient -e t >/dev/null 2>&1; then
    emacsclient -n -- "$f"     # reuse active frame if any
  else
    emacs -- "$f"
  fi
}

# # grope: cd to the dir of the first file whose contents match (ripgrep)
# grope() {
#   local f
#   IFS= read -r -d '' f < <(rg -uu -l -0 -- "$*" .)
#   [ -n "$f" ] && cd -- "$(dirname -- "$f")" || { echo "grope: no match" >&2; return 1; }
# }
# 
# # gropen: open the first matching file in Emacs (prefer existing server)
# gropen() {
#   local f
#   IFS= read -r -d '' f < <(rg -uu -l -0 -- "$*" .)
#   [ -z "$f" ] && { echo "gropen: no match" >&2; return 1; }
#   emacsclient -n -- "$f" 2>/dev/null || emacs -- "$f"
# }

# --- Doom Emacs daemon config ---
export DOOMDIR="$HOME/.config/doom"
export DOOM_INIT="$HOME/.config/doom-emacs"

# Start the daemon (idempotent)
doomd-start() {
  if emacsclient -s doom -e t >/dev/null 2>&1; then
    echo "Doom daemon already running."
  else
    emacs --daemon=doom --init-directory="$DOOM_INIT"
    echo "Doom daemon started."
  fi
}

# Stop the daemon
doomd-stop() {
  emacsclient -s doom -e '(kill-emacs)'
}

# Ping status
doomd-status() {
  if emacsclient -s doom -e '(emacs-version)' >/dev/null 2>&1; then
    echo "Doom daemon is up."
  else
    echo "Doom daemon is down."
  fi
}

# Open a terminal frame (TTY)
alias doomt='emacsclient -t -s doom'

# Open a GUI frame
alias doomc='emacsclient -c -n -s doom'

# One-shot, no daemon: run Doom in the terminal right now
alias doom-now='emacs --init-directory="$DOOM_INIT" -nw'


# [ -f "/home/atlas/.ghcup/env" ] && . "/home/atlas/.ghcup/env" # ghcup-env

# Created by `pipx` on 2025-08-06 21:41:24
export PATH="$PATH:/home/atlas/.local/bin"
export PATH="$HOME/.local/zig/0.15.1:$PATH"
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
