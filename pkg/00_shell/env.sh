################################################################################
# Variables
################################################################################

export USER="${USER:-$(whoami)}"
export GITUSER="${USER}"
export REPOS="${HOME}/repos"
export GHREPOS="${REPOS}/github.com/${GITUSER}"
# export SCRIPTS="${HOME}/scripts"
# export SNIPPETS="${HOME}/snippets"
# export HELP_BROWSER=lynx
# export DOTNET_ROOT="${HOME}/.local/share/dotnet"
# export PYENV_ROOT="$HOME/.local/share/pyenv"

export LANG=en_US.UTF-8

export BAT_THEME=gruvbox-dark
# export DENO_INSTALL="${XDG_DATA_HOME}/deno"
# export DENO_INSTALL_ROOT="${XDG_DATA_HOME}/deno/bin"
# export NVM_DIR="${XDG_DATA_HOME}/nvm"
# export RUSTUP_HOME="${XDG_DATA_HOME}/rustup"
# export CARGO_HOME="${XDG_DATA_HOME}/cargo"
# export VOLTA_HOME="${HOME}/.volta"
# export FNM_HOME="${HOME}/.local/share/fnm"
# export ZSH_HOME="${HOME}/.local/share/zsh"
# export PPM_HOME="${HOME}/.local/share/ppm"
# export GOPRIVATE="github.com/${GITUSER}/*,gitlab.com/${GITUSER}/*"
# export GOPATH="${XDG_DATA_HOME}/go"
# export GOBIN="${HOME}/.local/bin"
# export GOPROXY=direct
# export KUBECONFIG="${HOME}/.kube/config"
# export CGO_ENABLED=0
# export PYTHONDONTWRITEBYTECODE=2
# export LC_COLLATE=C
# #export CFLAGS="-Wall -Wextra -Werror -O0 -g -fsanitize=address -fno-omit-frame-pointer -finstrument-functions"
# export ZSH="${XDG_DATA_HOME}/oh-my-zsh"
# export ZSH_CUSTOM="${XDG_CONFIG_HOME}/oh-my-custom"
# export FLYCTL_INSTALL=${XDG_DATA_HOME}/.local/share/fly
# export PNPM_HOME="${XDG_DATA_HOME}/pnpm"
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

# remember last arg will be first in path. Precedence comes to the item that
# comes at the head of the list. Appended paths are pruned if they do not
# exist.
pathprepend "${HOME}/.local/share/fzf/bin"

# pathappend /Applications/kitty.app/Contents/MacOS

if [[ -d "${DOTFILES_DATA_HOME}/secrets" ]]; then
	for file in ${DOTFILES_DATA_HOME}/secrets/*; do
		# shellcheck source=${file} dynamically load secrets
		source "${file}"
	done
fi

# case "$-" in
# *i*)
# 	# This is an interactive shell (like when .zshrc is sourced)
# 	if [[ -n "$BASH_VERSION" ]]; then
# 		# This is Bash
# 		source_if "${DOTFILES_STATE_HOME}/pkg/shell/lib/bashrc"
# 	elif [[ -n "$ZSH_VERSION" ]]; then
# 		# This is Zsh
# 		source_if "${DOTFILES_STATE_HOME}/pkg/shell/lib/zshrc"
# 	# Add checks for other shells if needed, e.g.:
# 	# elif [[ -n "$KSH_VERSION" ]]; then
# 	# 	# This is Ksh
# 	# 	source_if "${DOTFILES_STATE_HOME}/pkg/shell/lib/kshrc"
# 	# elif [[ -n "$FISH_VERSION" ]]; then
# 	# 	# This is Fish (Fish doesn't typically source files like this, but for completeness)
# 	# 	source_if "${DOTFILES_STATE_HOME}/pkg/shell/lib/fishrc"
# 	else
# 		# Unknown shell or a shell without a standard version variable
# 		# Optional: log a warning or source a generic rc file
# 		# log_message WARN "Unknown shell. Not sourcing shell-specific rc file." >&2
# 		: # Do nothing
# 	fi
#
# 	;;
# *)
# 	# This is a non-interactive shell (e.g., running a script)
# 	;;
# esac
