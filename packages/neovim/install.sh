#!/usr/bin/env bash

set -o errexit  # exit if non-zero status code is returned
set -o nounset  # exit if undefined variable is used
set -o pipefail # exit if no-zero status code is returned in a pipeline

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)

# Define the locations
NVIM_DIR="${SCRIPT_DIR}/nvim"
CONFIG_DIR="${HOME}/.config"
NVIM_CONFIG_DIR="${CONFIG_DIR}/nvim"
NVIM_BACKUP_DIR="${CONFIG_DIR}/nvim-backup"

# Check if the ~/.config directory exists
if [[ ! -d "${CONFIG_DIR}" ]]; then
	echo "Creating ~/.config directory..."
	mkdir -p "${CONFIG_DIR}"
fi

# Backup and remove existing nvim config if it exists
if [[ -d "${NVIM_CONFIG_DIR}" || -L "${NVIM_CONFIG_DIR}" ]]; then
	echo "Backing up existing nvim configuration..."
	if [[ -d "${NVIM_BACKUP_DIR}" || -L "${NVIM_BACKUP_DIR}" ]]; then
		echo "Removing old backup..."
		rm -rf "${NVIM_BACKUP_DIR}"
	fi
	mv "${NVIM_CONFIG_DIR}" "${NVIM_BACKUP_DIR}"
fi

# Create symlink for nvim config
if [[ ! -L "${NVIM_CONFIG_DIR}" ]]; then
	echo "Creating symlink for nvim configuration..."
	ln -s "${NVIM_DIR}" "${NVIM_CONFIG_DIR}"
fi

# Detect operating system
DISTRO=""
case "$(uname -s)" in
Darwin)
	DISTRO="Darwin"
	echo "Darwin detected"
	;;
Linux)
	if [ -f /etc/os-release ]; then
		# Source the os-release file to use its variables
		# shellcheck disable=SC1091
		. /etc/os-release
		echo "Detected Linux Distribution: $NAME"
		echo "Version: $VERSION"
		DISTRO="Linux-$NAME"
	else
		echo "/etc/os-release file not found."
	fi
	;;
FreeBSD)
	DISTRO="FreeBSD"
	echo "FreeBSD detected"
	;;
*)
	echo "Unable to detect OS"
	;;
esac

case "${DISTRO}" in
Darwin)
	echo "${DISTRO} not currently supported"
	;;
FreeBSD)
	echo "${DISTRO} not currently supported"
	;;
Linux-Ubuntu | Linux-debian)
	apt-get update
	apt-get install neovim clang nodejs npm -y
	;;
Linux-Arch)
	echo "Arch not currently supported"
	;;
*)
	echo "${DISTRO} not currently supported"
	;;
esac

echo "Installation complete."
