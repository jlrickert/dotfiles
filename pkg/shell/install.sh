#!/usr/bin/env bash

install_symlink "${PACKAGE_ROOT}/bin" "${HOME}/bin"

if ! command -v starship &>/dev/null; then
	log_message INFO "starship not found. Installing..."
	curl -sS https://starship.rs/install.sh | sh
else
	log_message INFO "starship is already installed."
fi

if command -v brew &>/dev/null; then
	log_message INFO "Homebrew is installed."
	if ! command -v fzf &>/dev/null; then
		log_message INFO "fzf not found. Installing with Homebrew..."
		brew install fzf
	else
		log_message INFO "fzf is already installed."
	fi
else
	log_message INFO "Homebrew is not installed. Skipping fzf installation."
	# Optionally, add instructions on how to install brew or handle this case
	# log_message INFO "Please install Homebrew first: /bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
fi

blockinfile "${HOME}/.bashrc" bashrc ". ${PACKAGE_ROOT}/bashrc"
blockinfile "${HOME}/.zshenv" zshenv ". ${PACKAGE_ROOT}/env.sh"
blockinfile "${HOME}/.zshrc" zshrc ". ${PACKAGE_ROOT}/lib/zshrc"
