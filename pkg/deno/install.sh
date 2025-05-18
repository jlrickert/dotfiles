#!/usr/bin/env bash

PKG_ROOT="$(dirname "$(readlink -f "$0")")"
PROJECT_ROOT="$(cd "${PKG_ROOT}" && git rev-parse --show-toplevel)"

cd "${PKG_ROOT}" || return 1

. "${PROJECT_ROOT}/lib/env.sh"
. "${PROJECT_ROOT}/lib/func.sh"

ensure_environment

if command -v deno &>/dev/null; then
	log_message INFO "Deno is already installed."
	return 0
fi

# If we reach here, deno is not installed.
log_message INFO "Deno not found. Attempting installation..."
if curl -fsSL https://deno.land/install.sh | sh; then
	log_message SUCCESS "Deno installed successfully."
	return 0
else
	log_message ERROR "Failed to install Deno." >&2
	return 1
fi
