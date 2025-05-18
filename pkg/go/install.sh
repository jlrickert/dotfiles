#!/usr/bin/env bash

PKG_ROOT="$(dirname "$(readlink -f "$0")")"
PROJECT_ROOT="$(cd "${PKG_ROOT}" && git rev-parse --show-toplevel)"

cd "${PKG_ROOT}" || return 1

. "${PROJECT_ROOT}/lib/env.sh"
. "${PROJECT_ROOT}/lib/func.sh"

ensure_environment

# Check if Go is already installed
if command -v go &>/dev/null; then
	log_message INFO "Go is already installed."
	return 0 # Exit successfully if already present
fi

log_message INFO "Go not found. Attempting installation..."

# Detect OS
case "$(uname -s)" in
Darwin)
	# macOS
	log_message INFO "Detected macOS. Installing Go via Homebrew..."
	# Check if Homebrew is installed
	if ! command -v brew &>/dev/null; then
		log_message ERROR "Homebrew is not installed. Please install Homebrew first to install Go on macOS." >&2
		log_message INFO "See https://brew.sh/ for installation instructions."
		return 1
	fi

	# Install Go package
	log_message INFO "Running 'brew install go'..."
	if brew install go; then
		log_message SUCCESS "Go installed successfully via Homebrew."
		return 0
	else
		log_message ERROR "Failed to install Go via Homebrew." >&2
		return 1
	fi
	;;

Linux)
	# Check if it's Ubuntu/Debian based
	if [ ! -f /etc/os-release ]; then
		log_message ERROR "Detected Linux but could not determine distribution from /etc/os-release. Automatic Go installation is not supported." >&2
		log_message INFO "Please refer to the official Go documentation for manual installation instructions."
		return 1
	fi

	# Source os-release to get ID
	. /etc/os-release

	# Use bouncer pattern: If not Ubuntu/Debian, exit early
	if [[ "${ID}" != "ubuntu" && "${ID}" != "debian" ]]; then
		log_message ERROR "Detected Linux but not Ubuntu/Debian. Automatic Go installation is not supported for your distribution (${ID})." >&2
		log_message INFO "Please refer to the official Go documentation for manual installation instructions."
		return 1
	fi

	# If we reach here, it's Ubuntu/Debian. Proceed with installation.
	log_message INFO "Detected Ubuntu/Debian. Installing Go via apt..."

	# Update apt index and install golang package
	log_message INFO "Running 'sudo apt-get update && sudo apt-get install -y golang'..."
	if sudo apt-get update >/dev/null 2>&1 && sudo apt-get install -y golang >/dev/null 2>&1; then
		log_message SUCCESS "Go installed successfully via apt."
		return 0
	else
		log_message ERROR "Failed to install Go via apt." >&2
		return 1
	fi
	;;

*)
	log_message ERROR "Unsupported operating system: $(uname -s). Cannot automatically install Go." >&2
	log_message INFO "Please refer to the official Go documentation for manual installation instructions."
	return 1
	;;
esac
