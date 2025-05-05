#!/usr/bin/env bash
# Installs nvim configuration by symlink

declare NOCOLOR=
[[ -t 1 ]] || NOCOLOR=y
declare GREEN='[38;2;0;255;0m'
declare RESET='[0m'

if [[ -n "${NOCOLOR}" ]]; then
	GREEN=
	RESET=
fi

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
SOURCE_DIR="${SCRIPT_DIR}/src"
TARGET_DIR="${HOME}/.config/nvim"
BACKUP_DIR="${HOME}/.local/state/dotfiles/backups"
BACKUP_FILE="${BACKUP_DIR}/nvim" # Backup file name will be just "nvim"

# Check if the target directory already exists.
if [ -e "${TARGET_DIR}" ]; then
	# If it exists, check if it's a symlink.
	if [ -L "${TARGET_DIR}" ]; then
		# If it's a symlink, check if it points to the correct directory.
		if [ "$(readlink "${TARGET_DIR}")" = "${SOURCE_DIR}" ]; then
			echo "${GREEN}Symlink already exists and points to the correct directory: ${TARGET_DIR} -> ${SOURCE_DIR}${RESET}"
			exit 0 # Exit successfully if the symlink is already correct.
		else
			# If it's a symlink but points to the wrong directory, remove it.
			echo "${GREEN}Removing existing symlink: ${TARGET_DIR}${GREEN}"
			rm "${TARGET_DIR}"
		fi
	else
		# If it's not a symlink (i.e., it's a regular directory or file), back it up.
		echo "${GREEN}Backing up existing directory/file: ${TARGET_DIR} to ${BACKUP_FILE}${RESET}"
		mv "${TARGET_DIR}" "${BACKUP_FILE}"
	fi
fi

# Create the symlink.
echo "${GREEN}Creating symlink: ${TARGET_DIR} -> ${SOURCE_DIR}${RESET}"
ln -s "${SCRIPT_DIR}" "${TARGET_DIR}"
