#!/usr/bin/env bash
# Installs nvim configuration by symlink

SOURCE_DIR="${PACKAGE_ROOT}/src"
TARGET_DIR="${HOME}/.config/nvim"

# Use the reusable install_symlink function
install_symlink "${SOURCE_DIR}" "${TARGET_DIR}"

dotm install rust

if ! command -v stylua &>/dev/null; then
	log_message INFO "stylua not found. Installing..."
	cargo install stylua --features luajit
else
	log_message INFO "stylua is already installed."
fi

log_message INFO "Neovim symlink installation script finished."
