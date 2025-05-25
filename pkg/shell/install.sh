#!/usr/bin/env bash

install_symlink "${PACKAGE_ROOT}/bin" "${HOME}/bin"

if ! command -v starship &>/dev/null; then
	log_message INFO "starship not found. Installing..."
	curl -sS https://starship.rs/install.sh | sh -s -- -y
else
	log_message INFO "starship is already installed."
fi

if ! command -v fzf &>/dev/null; then
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
		git clone --depth 1 https://github.com/junegunn/fzf.git "${HOME}/.local/share/fzf"
		cd "${HOME}/.local/share/fzf" || exit 1
		./install --no-update-rc
	fi
fi

if [ ! -d "${HOME}/.local/share/oh-my-zsh" ]; then
	git clone https://github.com/ohmyzsh/ohmyzsh.git "${HOME}/.local/share/oh-my-zsh"
fi

blockinfile "${HOME}/.bashrc" bashrc ". ${PACKAGE_ROOT}/lib//bashrc"
blockinfile "${HOME}/.zshrc" zshrc ". ${PACKAGE_ROOT}/lib/zshrc"
