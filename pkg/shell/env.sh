################################################################################
# Utils
################################################################################

_source_if() { [[ -r "$1" ]] && source "$1"; }
_have() { type "$1" &>/dev/null; }

################################################################################
# Variables
################################################################################

export USER="${USER:-$(whoami)}"
export GITUSER="${USER}"
export REPOS="${HOME}/repos"
export GHREPOS="${REPOS}/github.com/${GITUSER}"
export SCRIPTS="${HOME}/scripts"
export SNIPPETS="${HOME}/snippets"
export HELP_BROWSER=lynx
export DOTNET_ROOT="${HOME}/.local/share/dotnet"
export PYENV_ROOT="$HOME/.local/share/pyenv"

export LANG=en_US.UTF-8

export BAT_THEME=gruvbox-dark
export DENO_INSTALL="${XDG_DATA_HOME}/deno"
export DENO_INSTALL_ROOT="${XDG_DATA_HOME}/deno/bin"
export NVM_DIR="${XDG_DATA_HOME}/nvm"
export RUSTUP_HOME="${XDG_DATA_HOME}/rustup"
export CARGO_HOME="${XDG_DATA_HOME}/cargo"
export VOLTA_HOME="${HOME}/.volta"
export FNM_HOME="${HOME}/.local/share/fnm"
export ZSH_HOME="${HOME}/.local/share/zsh"
export PPM_HOME="${HOME}/.local/share/ppm"
export GOPRIVATE="github.com/${GITUSER}/*,gitlab.com/${GITUSER}/*"
export GOPATH="${XDG_DATA_HOME}/go"
export GOBIN="${HOME}/.local/bin"
export GOPROXY=direct
export KUBECONFIG="${HOME}/.kube/config"
export CGO_ENABLED=0
export PYTHONDONTWRITEBYTECODE=2
export LC_COLLATE=C
#export CFLAGS="-Wall -Wextra -Werror -O0 -g -fsanitize=address -fno-omit-frame-pointer -finstrument-functions"
export ZSH="${XDG_DATA_HOME}/oh-my-zsh"
export ZSH_CUSTOM="${XDG_CONFIG_HOME}/oh-my-custom"
export FLYCTL_INSTALL=${XDG_DATA_HOME}/.local/share/fly
export PNPM_HOME="${XDG_DATA_HOME}/pnpm"
export CDPATH="${CDPATH:-}:${HOME}/repos/github.com/${USER}/"

################################################################################
# Fuzzy finder
################################################################################
export FZF_DEFAULT_COMMAND='rg --files --hidden --color=never'
# export FZF_DEFAULT_OPTS="--ansi --phony --preview='bat --color=always {}'"
# export FZF_ALT_C_COMMAND='fd --color=never --type d'
export FZF_ALT_C_COMMAND='fdfind --color=never --type d'
export FZF_ALT_C_OPTS="--preview 'tree -C {}"

# Options to fzf command
# export FZF_COMPLETION_OPTS='--border --info=inline'

################################################################################
# Path
################################################################################
# append to path if it exists
function pathappend() {
	for arg in "$@"; do
		test -d "$arg" || continue
		PATH=${PATH//":$arg:"/:} # remove all occuences of `:$arg:` from the PATH
		PATH=${PATH/#"$arg:"/}   # remove `"$arg:"` from the the beginning of the string
		PATH=${PATH/%":$arg"/}   # remove `":$arg` from the end of the string
		export PATH="${PATH:+"$PATH:"}$arg"
	done
}

# prepend to path if it exists
function pathprepend() {
	for arg in "$@"; do
		test -d "$arg" || continue
		PATH=${PATH//:"$arg:"/:}
		PATH=${PATH/#"$arg:"/}
		PATH=${PATH/%":$arg"/}
		export PATH="$arg${PATH:+":${PATH}"}"
	done
}

# remember last arg will be first in path. Precedence comes to the item that comes at the head of the list. Appended paths are pruned if they do not exist.
pathprepend \
	"${HOME}/.local/share/fzf/bin"

pathappend /Applications/kitty.app/Contents/MacOS

if [[ -d "${DOTFILES_DATA_HOME}/secrets" ]]; then
	for file in ${DOTFILES_DATA_HOME}/secrets/*; do
		# shellcheck source=${file} dynamically load secrets
		source "${file}"
	done
fi
