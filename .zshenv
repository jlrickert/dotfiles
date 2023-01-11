_have()      { type "$1" &>/dev/null; }
_source_if() { [[ -r "$1" ]] && source "$1"; }

export USER="${USER:-$(whoami)}"
export GITUSER="$USER"
export REPOS="$HOME/Repos"
export GHREPOS="$REPOS/github.com/$GITUSER"
export DOTFILES="$GHREPOS/dotfiles"
export SCRIPTS="$HOME/Scripts"
export SNIPPETS="$DOTFILES/snippets"
export HELP_BROWSER=lynx
export DESKTOP="$HOME/Desktop"
export DOCUMENTS="$HOME/Documents"
export DOWNLOADS="$HOME/Downloads"

export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:="$HOME/.cache"}
export XDG_STATE_HOME=${XDG_STATE_HOME:="$HOME/.local/state"}

export TEMPLATES="$HOME/Templates"
export PUBLIC="$HOME/Public"
export PRIVATE="$HOME/Private"
export PICTURES="$HOME/Pictures"
export MUSIC="$HOME/Music"
export VIDEOS="$HOME/Videos"
export ZETDIR="$GHREPOS/zet"
export TERM=xterm-256color
export HRULEWIDTH=73
_have nano && export EDITOR=nano
_have vi && export EDITOR=vi
_have vim && export EDITOR=vim
_have nvim && export EDITOR=nvim
_have vim && export VISUAL=vim
_have nvim && export VISUAL=nvim
_have vim && export EDITOR_PREFIX=vim
export DENO_INSTALL="$XDG_DATA_HOME/deno"
export NVM_DIR="$XDG_DATA_HOME/nvm"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export GOPRIVATE="github.com/$GITUSER/*,gitlab.com/$GITUSER/*"
export GOPATH="$XDG_DATA_HOME/go"
export GOBIN="$HOME/.local/bin"
#export GOPROXY=direct
export CGO_ENABLED=0
export PYTHONDONTWRITEBYTECODE=2
export LC_COLLATE=C
#export CFLAGS="-Wall -Wextra -Werror -O0 -g -fsanitize=address -fno-omit-frame-pointer -finstrument-functions"
export ZSH="$XDG_DATA_HOME/oh-my-zsh"
export VOLTA_HOME="$HOME/.volta"

# PERSONAL=$XDG_CONFIG_HOME/personal
# source $PERSONAL/env
# for i in $(find -L $PERSONAL); do
#     source "$i"
# done
# source /usr/share/doc/fzf/examples/key-bindings.zsh
# source /usr/share/doc/fzf/examples/completion.zsh

export GIT_EDITOR=$EDITOR
export FLYCTL_INSTALL=$XDG_DATA_HOME/.fly

# # Where should I put you?

# export KEYTIMEOUT=1
# bindkey -s ^f "tmux-sessionizer\n"
# bindkey '^[p' up-history
# bindkey '^[n' down-history
# bindkey '^[r' history-incremental-search-backward
# bindkey -M vicmd '^[n' down-history
# bindkey -M vicmd '^[p' up-history
# bindkey -M vicmd 'k' up-line
# bindkey -M vicmd 'j' down-line
# bindkey '^ ' autosuggest-accept

# ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#262626"

typeset -aU path
path=(
    "$HOME/.local/bin"
    "$FLYCTL_INSTALL/bin"
    "$HOME/.node_modules/bin"
    "$DENO_INSTALL/bin"
    "$CARGO_HOME/bin"
    "$VOLTA_HOME/bin"
    "$SCRIPTS"
    "$path[@]"
)

alias vim=nvim
