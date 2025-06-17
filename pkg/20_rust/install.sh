#!/usr/bin/env bash

export CARGO_HOME="${XDG_DATA_HOME}/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

# Check if Rust (cargo) is already installed
if command -v cargo &>/dev/null; then
	log_message SUCCESS "Rust (cargo) is already installed. Exiting installation script."
	exit 0
fi

log_message INFO "Rust (cargo) not found. Proceeding with installation."

log_message INFO "Downloading Rustup installation script..."

temp_dir=$(mktemp -d)
if [ ! -d "$temp_dir" ]; then
	log_message ERROR "Failed to create temporary directory for Rustup script download." >&2
	exit 1
fi

# Ensure cleanup happens on exit or signal
trap 'log_message INFO "Cleaning up temporary Rustup script directory: $temp_dir"; rm -rf "$temp_dir"' EXIT INT TERM

# Download the script into the temporary directory
if ! curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs >"$temp_dir/rustup.sh"; then
	log_message ERROR "Failed to download Rustup installation script to $temp_dir." >&2
	# The trap will handle cleanup
	exit 1
fi

log_message SUCCESS "Rustup installation script downloaded."

log_message INFO "Running Rustup installation script..."
if ! bash ./rustup.sh -y; then
	log_message ERROR "Failed to execute Rustup installation script." >&2
	exit 1
fi
log_message SUCCESS "Rustup installed successfully."
