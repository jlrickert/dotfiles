#!/usr/bin/env bash
# Bootstraps myt dotfiles from the interwebs. runable with sh -c "$(curl -fsLS
# https://raw.githubusercontent.com/jlrickert/dotfiles/main/setup.sh)"

set -o errexit # exit if non-zero status code is returned
set -o nounset # exit if undefined variable is used

export NOCOLOR=
[[ -t 1 ]] || NOCOLOR=y
export GOLD='[38;2;184;138;0m'
export RED='[38;2;255;0;0m'
export GREY='[38;2;100;100;100m'
export CYAN='[38;2;0;255;255m'
export GREEN='[38;2;0;255;0m'
export RESET='[0m'

if [[ -n "$NOCOLOR" ]]; then
	GOLD=
	RED=
	GREY=
	CYAN=
	GREEN=
	RESET=
fi

export DOTFILES_VERSION="${DOTFILES_VERSION:$(git rev-parse --abbrev-ref HEAD)}"

# This is where the user specific config should live that is private. This is
# safe to sync between systems
export DOTFILES_DATA_HOME="${DOTFILES_DATA:-${HOME}/.local/share/dotfiles}"

# Dotfiles specific state for the machine. This is not safe to sync between
# systems.
export DOTFILES_STATE_HOME="${DOTFILES_STATE:-${HOME}/.local/state/dotfiles}"

# This is where the publish shared configuration lives.  This is where
export DOTFILES_CONFIG_HOME="${DOTFILES_CONFIG:-${HOME}/.config/dotfiles}"

# This is where cache data lives. Safe to remove at anytime.
export DOTFILES_CACHE_HOME="${DOTFILES_CACHE:-${HOME}/.cache/dotfiles}"

mkdir -p "${DOTFILES_DATA_HOME}"
mkdir -p "${DOTFILES_STATE_HOME}"
mkdir -p "${DOTFILES_CONFIG_HOME}"
mkdir -p "${DOTFILES_CACHE_HOME}"

if [ ! -d "${DOTFILES_CONFIG_HOME}" ]; then
	git clone https://github.com/jlrickert/dotfiles "${DOTFILES_CONFIG_HOME}"
fi

git checkout "${DOTFILES_VERSION}"

cd "${DOTFILES_CONFIG_HOME}"

source env
dotsh boostrap
