#!/usr/bin/env bash

install_symlink "${PACKAGE_ROOT}/bin" "${HOME}/bin"

if ! have starship && have brew; then
	log_message INFO "starship not found. Installing with Homebrew..."
	brew install starship
elif ! have starship; then
	log_message INFO "starship not found. Installing..."
	curl -sS https://starship.rs/install.sh | sh -s -- -y
else
	log_message INFO "starship is already installed."
fi

if ! have fzf && have brew; then
	log_message INFO "fzf not found. Installing with Homebrew..."
	brew install fzf
elif ! have fzf; then
	log_message INFO "Homebrew is not installed. Skipping fzf installation."
	git clone --depth 1 https://github.com/junegunn/fzf.git "${XDG_DATA_HOME}/fzf"
	cd "${XDG_DATA_HOME}/fzf" || exit 1
	./install --bin
else
	log_message INFO "fzf is already installed."
fi

# if [ ! -d "${XDG_DATA_HOME}/oh-my-zsh" ]; then
# 	git clone https://github.com/ohmyzsh/ohmyzsh.git "${XDG_DATA_HOME}/oh-my-zsh"
# fi

if [[ -d "${HOME}/repo" ]]; then
	mkdir -p "${HOME}/repo"
fi
