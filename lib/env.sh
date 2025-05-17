#!/usr/bin/env bash

export NOCOLOR=
[[ -t 1 ]] || NOCOLOR=y
export GOLD='[38;2;184;138;0m'
export RED='[38;2;255;0;0m'
export GREY='[38;2;100;100;100m'
export CYAN='[38;2;0;255;255m'
export GREEN='[38;2;0;255;0m'
export RESET='[0m'

if [[ -n "${NOCOLOR}" ]]; then
	GOLD=
	RED=
	GREY=
	CYAN=
	GREEN=
	RESET=
fi

PROJECT_ROOT="$(cd "${SCRIPT_DIR}" && git rev-parse --show-toplevel)"

export DOTFILES_VERSION="${DOTFILES_VERSION:-$(git rev-parse --abbrev-ref HEAD)}"

# This is where the user specific config should live that is private. This is
# safe to sync between systems
export DOTFILES_DATA_HOME="${DOTFILES_DATA_HOME:-${HOME}/.local/share/dotfiles}"

# Dotfiles specific state for the machine. This is not safe to sync between
# systems.
export DOTFILES_STATE_HOME="${DOTFILES_STATE_HOME:-${HOME}/.local/state/dotfiles}"

# This is where the publish shared configuration lives.  This is where
export DOTFILES_CONFIG_HOME="${DOTFILES_CONFIG_HOME:-${HOME}/.config/dotfiles}"

# This is where cache data lives. Safe to remove at anytime.
export DOTFILES_CACHE_HOME="${DOTFILES_CACHE_HOME:-${HOME}/.cache/dotfiles}"

# Sets and exports the DOTFILES_LOG_FILE variable.
# Defaults to a 'log' file within the DOTFILES_STATE directory if not already set.
export DOTFILES_LOG_FILE="${DOTFILES_LOG_FILE:-${DOTFILES_STATE_HOME}/log}"

if [ -f "${PROJECT_ROOT}/.env" ]; then
	. "${PROJECT_ROOT}/.env"
fi
