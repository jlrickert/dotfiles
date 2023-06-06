export USER="${USER:-$(whoami)}"
export GITUSER="$USER"
export REPOS="$HOME/repos"
export PERSONAL="$HOME/personal"
export DOTFILES="$PERSONAL/dotfiles"
export SCRIPTS="$HOME/scripts"
export SNIPPETS="$HOME/snippets"
export HELP_BROWSER=lynx

export DESKTOP="$HOME/Desktop"
export DOCUMENTS="$HOME/Documents"
export DOWNLOADS="$HOME/Downloads"
export PUBLIC="$HOME/Public"
export PRIVATE="$HOME/Private"
export PICTURES="$HOME/Pictures"
export TEMPLATES="$HOME/Templates"
export MUSIC="$HOME/Music"
export VIDEOS="$HOME/Videos"

export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:="$HOME/.cache"}
export XDG_STATE_HOME=${XDG_STATE_HOME:="$HOME/.local/state"}

export ZETDIR="$GHREPOS/zet"
export TERM=xterm-256color
export HRULEWIDTH=73
export DENO_INSTALL="$XDG_DATA_HOME/deno"
export NVM_DIR="$XDG_DATA_HOME/nvm"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export VOLTA_HOME="$HOME/.volta"
export FNM_HOME="$HOME/.local/share/fnm" # Figure out how to change this to use XDG_DATA_HOME
export ZSH_HOME="$HOME/.local/share/zsh"
export GOPRIVATE="github.com/$GITUSER/*,gitlab.com/$GITUSER/*"
export GOPATH="$XDG_DATA_HOME/go"
export GOBIN="$HOME/.local/bin"
#export GOPROXY=direct
export KUBECONFIG="$HOME/.kube/config"
export CGO_ENABLED=0
export PYTHONDONTWRITEBYTECODE=2
export LC_COLLATE=C
#export CFLAGS="-Wall -Wextra -Werror -O0 -g -fsanitize=address -fno-omit-frame-pointer -finstrument-functions"
export ZSH="$XDG_DATA_HOME/oh-my-zsh"

# PERSONAL=$XDG_CONFIG_HOME/personal
# source $PERSONAL/env
# for i in $(find -L $PERSONAL); do
#     source "$i"
# done
# source /usr/share/doc/fzf/examples/key-bindings.zsh
# source /usr/share/doc/fzf/examples/completion.zsh

export FLYCTL_INSTALL=$XDG_DATA_HOME/.fly

export FZF_DEFAULT_COMMAND='rg --files --hidden --color=never'
# export FZF_DEFAULT_OPTS="--ansi --phony --preview='bat --color=always {}'"
# export FZF_ALT_C_COMMAND='fd --color=never --type d'
export FZF_ALT_C_COMMAND='fdfind --color=never --type d'
export FZF_ALT_C_OPTS="--preview 'tree -C {}"

# Options to fzf command
# export FZF_COMPLETION_OPTS='--border --info=inline'

# ZSH magic to set the $PATH variable
typeset -U path
path+=(
  "$SCRIPTS"
  "$HOME/.local/bin"
  "$HOME/.local/share/flutter/bin"
  "$CARGO_HOME/bin"
  "$FNM_HOME"
  /usr/local/go/bin
  /usr/local/bin
  /usr/local/bin
  /usr/local/sbin
  /usr/local/games
  /usr/games
  /usr/sbin
  /usr/bin
  /snap/bin
  /sbin
  /bin
)

# export PATH="$HOME/.local/bin:$FLYCTL_INSTALL/bin:$DENO_INSTALL/bin:$CARGO_HOME/bin:$VNM_HOME/bin:$SCRIPTS:${FNM_HOME}:${PATH}"

# [[ -x "$(which nano)" ]] && export EDITOR=nano
# [[ -x "$(which vi)" ]] && export EDITOR=vi
# [[ -x "$(which vim)" ]] && export EDITOR=vim
[[ -x "$(which nvim)" ]] && export EDITOR=nvim
[[ -x "$(which vim)" ]] && export VISUAL=vim
[[ -x "$(which nvim)" ]] && export VISUAL=nvim
[[ -x "$(which vi)" ]] && export EDITOR_PREFIX=vi
[[ -x "$(which vi)" ]] && export EDITOR_PREFIX=vi
[[ -x "$(which vim)" ]] && export EDITOR_PREFIX=vim
[[ -x "$(which nvim)" ]] && export EDITOR_PREFIX=nvim
export GIT_EDITOR=$EDITOR

export fpath=("${ZSH_HOME}/completions" $fpath)
