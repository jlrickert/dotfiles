#!/usr/bin/env bash

ensure_environment

if command -v deno &>/dev/null; then
	log_message INFO "Deno is already installed."
	exit 0
fi

# If we reach here, deno is not installed.
log_message INFO "Deno not found. Attempting installation..."
if curl -fsSL https://deno.land/install.sh | DENO_INSTALL="$HOME/.local/share/deno" sh -s -- -y; then
	log_message SUCCESS "Deno installed successfully."
	exit 0
else
	log_message ERROR "Failed to install Deno." >&2
	exit 1
fi
