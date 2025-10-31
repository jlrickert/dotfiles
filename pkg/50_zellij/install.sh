#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

################################################################################
# Zellij Installation Script
################################################################################

PKG_ROOT="$(dirname "$(readlink -f "$0")")"
PROJECT_ROOT="$(cd "${PKG_ROOT}" && git rev-parse --show-toplevel)"

ensure_environment

################################################################################
# Helper Functions
################################################################################

_install_brew_package() {
	local package=$1
	if ! have brew; then
		return 1
	fi
	brew install "${package}"
}

_install_go_package() {
	local package=$1
	if ! have go; then
		return 1
	fi
	go install "${package}"
}

_install_with_fallback() {
	local name=$1
	local brew_pkg=$2
	local go_pkg=$3

	if have "${name}"; then
		log_message INFO "${name} is already installed."
		return 0
	fi

	if _install_brew_package "${brew_pkg}"; then
		log_message SUCCESS "${name} installed via Homebrew."
		return 0
	fi

	if _install_go_package "${go_pkg}"; then
		log_message SUCCESS "${name} installed via Go."
		return 0
	fi

	log_message WARN "Failed to install ${name}."
	return 1
}

################################################################################
# Package Installation
################################################################################

install_symlink "${PACKAGE_ROOT}/config.kdl" \
	"${XDG_CONFIG_HOME}/zellij/config.kdl"

install_symlink "${PACKAGE_ROOT}/local.zellij-theme-switcher.plist" \
	"${HOME}/Library/LaunchAgents/local.zellij-theme-switcher.plist"

if ! have zellij; then
	log_message INFO "Installing zellij..."
	if have brew; then
		brew install zellij
	else
		log_message INFO "Installing zellij from source..."
		curl -L \
			https://github.com/zellij-org/zellij/releases/download/v0.43.1/zellij-x86_64-unknown-linux-musl.tar.gz \
			-o /tmp/zellij.tar.gz
		tar xzf /tmp/zellij.tar.gz -C "${HOME}/.local/bin"
		rm /tmp/zellij.tar.gz
	fi
else
	log_message INFO "zellij is already installed."
fi

if have zellij; then
	mkdir -p "${DOTFILES_CACHE_HOME}/zsh/completions"
	zellij setup --generate-completion zsh > \
		"${DOTFILES_CACHE_HOME}/zsh/completions/_zellij"
	log_message SUCCESS "Zellij completions generated."
fi

log_message SUCCESS "Installation complete."
