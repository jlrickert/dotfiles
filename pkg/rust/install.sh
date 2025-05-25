#!/usr/bin/env bash

# Check if Rust (cargo) is already installed
if command -v cargo &>/dev/null; then
	log_message SUCCESS "Rust (cargo) is already installed. Exiting installation script."
	exit 0
fi

log_message INFO "Rust (cargo) not found. Proceeding with installation."

log_message INFO "Downloading Rustup installation script..."
if ! curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs >./rustup.sh; then
	log_message ERROR "Failed to download Rustup installation script." >&2
	exit 1
fi
log_message SUCCESS "Rustup installation script downloaded."

log_message INFO "Running Rustup installation script..."
if ! bash ./rustup.sh -y; then
	log_message ERROR "Failed to execute Rustup installation script." >&2
	# Attempt to clean up the script even on failure
	log_message INFO "Cleaning up temporary Rustup script..."
	if rm ./rustup.sh; then
		log_message SUCCESS "Temporary Rustup script cleaned up."
	else
		log_message WARN "Failed to remove temporary Rustup script: ./rustup.sh" >&2
	fi
	exit 1
fi
log_message SUCCESS "Rustup installed successfully."

log_message INFO "Cleaning up temporary Rustup script..."
if rm ./rustup.sh; then
	log_message SUCCESS "Temporary Rustup script cleaned up."
else
	log_message WARN "Failed to remove temporary Rustup script: ./rustup.sh" >&2
fi
