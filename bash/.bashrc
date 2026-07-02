#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

# FZF
eval "$(fzf --bash)"
export FZF_DEFAULT_OPTS="--height=40% --layout=reverse --border --preview 'bat --style=numbers --color=always {} | head -500'"

# Browser
export BROWSER="vivaldi-stable"

# Editor
# export EDITOR="doom-now"

# Aliases
alias cmacs='emacsclient -c -a ""'
alias tmacs='emacsclient -t'
# alias doom-reload='doom sync && pkill -f emacs && sleep 1 && emacs --daemon'

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

stowcfg() {
  local app="$1"
  mkdir -p "$HOME/.dotfiles/$app/.config"
  mv "$HOME/.config/$app" "$HOME/.dotfiles/$app/.config/" || return 1
  (cd "$HOME/.dotfiles" && stow "$app")
}

# --- Doom Emacs daemon config ---
export DOOMDIR="$HOME/.config/doom"
export DOOM_INIT="$HOME/.config/emacs"

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
# 
# # Open a terminal frame (TTY)
# alias doomt='emacsclient -t -s doom'
# 
# # Open a GUI frame
# alias doomc='emacsclient -c -n -s doom'
# 
# # One-shot, no daemon: run Doom in the terminal right now
# alias doom-now='emacs --init-directory="$DOOM_INIT" -nw'

# Created by `pipx` on 2025-08-06 21:41:24
export PATH="$PATH:/home/atlas/.local/bin"
export PATH="$HOME/.local/zig/0.15.1:$PATH"
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Added by LM Studio CLI tool (lms)
export PATH="$PATH:/home/atlas/.lmstudio/bin"
