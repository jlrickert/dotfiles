#!/usr/bin/env bash

install_symlink "${PACKAGE_ROOT}/bin" "${HOME}/bin"
install_symlink "${PACKAGE_ROOT}/starship.toml" "${XDG_CONFIG_HOME}/starship.toml"

if ! have starship && have brew; then
	log_message INFO "starship not found. Installing with Homebrew..."
	brew install starship
elif ! have starship; then
	log_message INFO "starship not found. Installing..."
	curl -sS https://starship.rs/install.sh | sh -s -- -y
else
	log_message INFO "starship is already installed."
fi

if ! have bat && have brew; then
	log_message INFO "bat not found. Installing with Homebrew..."
	brew install bat
elif ! have bat; then
	log_message INFO "bat is not avaiable."
else
	log_message INFO "bat is already installed."
fi

if ! have fd && have brew; then
	log_message INFO "fd not found. Installing with Homebrew..."
	brew install fd
elif ! have bat; then
	log_message INFO "Homebrew is not installed. Skipping fd installation."
else
	log_message INFO "bat is already installed."
fi

if ! have fzf && have brew; then
	log_message INFO "fzf not found. Installing with Homebrew..."
	brew install fzf
elif ! have fzf; then
	log_message INFO "Homebrew is not installed. Install fzf from source."
	git clone --depth 1 https://github.com/junegunn/fzf.git "${XDG_DATA_HOME}/fzf"
	cd "${XDG_DATA_HOME}/fzf" || exit 1
	./install --bin
else
	log_message INFO "fzf is already installed."
fi

if [[ -d "${HOME}/repo" ]]; then
	mkdir -p "${HOME}/repo"
fi
