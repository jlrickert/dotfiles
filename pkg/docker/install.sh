#!/usr/bin/env bash

PKG_ROOT="$(dirname "$(readlink -f "$0")")"
PROJECT_ROOT="$(cd "${PKG_ROOT}" && git rev-parse --show-toplevel)"

cd "${PKG_ROOT}" || return 1

. "${PROJECT_ROOT}/lib/env.sh"
. "${PROJECT_ROOT}/lib/func.sh"

ensure_environment

# Check if Docker is already installed
if command -v docker &>/dev/null; then
	log_message INFO "Docker is already installed."
	return 0 # Exit successfully if already present
fi

log_message INFO "Docker not found. Attempting installation..."

# Detect OS
case "$(uname -s)" in
Darwin)
	# macOS
	log_message INFO "Detected macOS."
	log_message WARN "Automatic Docker installation on macOS without Homebrew is not supported by this script." >&2
	log_message INFO "Please download and install Docker Desktop manually from the official website:"
	log_message INFO "  https://docs.docker.com/desktop/install/mac-install/"
	return 1 # Indicate that automatic installation failed for this OS/method
	;;

Linux)
	# Check if it's Ubuntu/Debian based
	if [ ! -f /etc/os-release ]; then
		log_message ERROR "Detected Linux but could not determine distribution from /etc/os-release. Automatic Docker installation is not supported." >&2
		log_message INFO "Please refer to the official Docker documentation for manual installation instructions."
		return 1
	fi

	# Source os-release to get ID and VERSION_CODENAME
	. /etc/os-release

	# Use bouncer pattern: If not Ubuntu/Debian, exit early
	if [[ "${ID}" != "ubuntu" && "${ID}" != "debian" ]]; then
		log_message ERROR "Detected Linux but not Ubuntu/Debian. Automatic Docker installation is not supported for your distribution (${ID})." >&2
		log_message INFO "Please refer to the official Docker documentation for manual installation instructions."
		return 1
	fi

	# If we reach here, it's Ubuntu/Debian. Proceed with installation.
	log_message INFO "Detected Ubuntu/Debian. Installing Docker Engine..."

	# Add Docker's official GPG key:
	log_message INFO "Adding Docker GPG key..."
	if ! sudo apt-get update >/dev/null 2>&1 ||
		! sudo apt-get install -y ca-certificates curl gnupg >/dev/null 2>&1 ||
		! sudo install -m 0755 -d /etc/apt/keyrings >/dev/null 2>&1 ||
		! curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg >/dev/null 2>&1 ||
		! sudo chmod a+r /etc/apt/keyrings/docker.gpg >/dev/null 2>&1; then
		log_message ERROR "Failed to add Docker GPG key." >&2
		return 1
	fi
	log_message SUCCESS "Docker GPG key added."

	# Add the repository to Apt sources:
	log_message INFO "Adding Docker repository..."
	echo \
		"deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
		  $(. /etc/os-release && echo "${VERSION_CODENAME}") stable" |
		sudo tee /etc/apt/sources.list.d/docker.list >/dev/null || {
		log_message ERROR "Failed to add Docker repository." >&2
		return 1
	}
	log_message SUCCESS "Docker repository added."

	# Install Docker packages
	log_message INFO "Updating apt index and installing Docker packages..."
	if ! sudo apt-get update >/dev/null 2>&1 ||
		! sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin >/dev/null 2>&1; then
		log_message ERROR "Failed to install Docker packages." >&2
		return 1
	fi
	log_message SUCCESS "Docker Engine installed successfully."
	log_message INFO "To run Docker as a non-root user, consider adding your user to the 'docker' group:"
	log_message INFO "  sudo usermod -aG docker \$USER"
	log_message INFO "You may need to log out and log back in for the group change to take effect."

	return 0
	;;
*)
	log_message ERROR "Unsupported operating system: $(uname -s). Cannot automatically install Docker." >&2
	log_message INFO "Please refer to the official Docker documentation for manual installation instructions."
	return 1
	;;
esac
