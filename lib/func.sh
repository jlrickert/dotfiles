PROJECT_ROOT="$(cd "${SCRIPT_DIR}" && git rev-parse --show-toplevel)"
. "${PROJECT_ROOT}/lib/env.sh"

# backup_item SOURCE_PATH
# Backs up an item (file or directory) to the configured dotfiles backup location.
# The backup location is derived from DOTFILES_STATE_HOME.
#
# Arguments:
#   $1: SOURCE_PATH - The path to the item (file or directory) to be backed up.
#
# Side effects:
#   - Creates the backup directory if it does not exist.
#   - Moves the SOURCE_PATH to the backup directory, renaming it to its basename.
#   - Writes messages to the standard output and the log file using log_message.
#
# Returns:
#   0: The item was successfully backed up.
#   1: An error occurred (e.g., DOTFILES_STATE_HOME not set, failed to create directory, failed to move).
#
function backup_item() {
	local source_path="$1"
	# Define the backup root relative to DOTFILES_STATE
	local backup_root_dir="${DOTFILES_STATE_HOME}/backups"
	local source_basename="$(basename "${source_path}")"
	# Backup path includes the basename of the source item
	local backup_target_path="${backup_root_dir}/${source_basename}"

	# Validate input
	if [ -z "${source_path}" ]; then
		log_message ERROR "backup_item requires a source path." >&2
		return 1
	fi
	# Ensure DOTFILES_STATE_HOME is set (basic check for backup location)
	if [ -z "${DOTFILES_STATE_HOME:-}" ]; then
		log_message ERROR "DOTFILES_STATE_HOME is not set. Cannot perform backup." >&2
		return 1
	fi

	log_message INFO "Attempting to back up: \`${source_path}\` to \`${backup_target_path}\`"

	# Ensure backup directory exists
	if mkdir -p "${backup_root_dir}"; then
		log_message INFO "Ensured backup directory exists: \`${backup_root_dir}\`"
	else
		log_message ERROR "Failed to create backup directory: \`${backup_root_dir}\`" >&2
		return 1
	fi

	# Perform the backup (move the item)
	if cp "${source_path}" "${backup_target_path}"; then
		log_message SUCCESS "Backed up item: \`${source_path}\` to \`${backup_target_path}\`"
		return 0 # Success
	else
		log_message ERROR "Failed to backup item: \`${source_path}\` to \`${backup_target_path}\`" >&2
		return 1 # Failure
	fi
}

export -f backup_item

# Ensures that core dotfiles directories and the log file location exist or can
# be created based on environment variables from lib/env.sh. This function
# first ensures the directory for the log file exists so that subsequent
# log_message calls can write to the file. It then attempts to create other
# necessary directories using mkdir -p. If unable to create any required
# directory, it logs an ERROR and returns 1. Returns 0 on success if all
# directories are ensured.
#
# Side effects:
# - Creates directories specified by $(dirname DOTFILES_LOG_FILE),
#   DOTFILES_STATE, DOTFILES_DATA, DOTFILES_CONFIG, and DOTFILES_CACHE.
# - Writes messages to the standard output and the file specified by DOTFILES_LOG_FILE
#   using the log_message function.
#
# Returns:
#   0: All required directories were ensured successfully.
#   1: Failed to create or ensure one or more required directories.
#
function ensure_environment() {
	# --- Critical First Step: Ensure the log file directory exists ---
	# We cannot use log_message to the *file* reliably until this directory
	# exists. We can still echo to stdout/stderr for immediate user feedback.

	local log_dir="$(dirname "${DOTFILES_LOG_FILE:-}")" # Use :- to handle case where DOTFILES_LOG_FILE is unset/empty

	if [ -z "${log_dir}" ] || [ "${log_dir}" = "." ]; then
		# If DOTFILES_LOG_FILE was empty or relative path resolving to '.',
		# this might indicate a problem or a setup where log isn't file based.
		# For robust file logging, a proper path is needed.
		# If this case is expected (e.g., logging only to stdout), adjust logic.
		# Assuming file logging is intended:
		echo -e "${RED}Error: DOTFILES_LOG_FILE is not set or is invalid (${DOTFILES_LOG_FILE:-}). Cannot determine log directory.${RESET}" >&2
		return 1 # Cannot proceed without a valid log file path
	fi

	# Temporarily use echo with fallback colors just in case log_message to
	# file fails (though it shouldn't fail after mkdir -p below).
	echo -e "${CYAN}Ensuring log directory exists: ${log_dir}${RESET}"

	if ! mkdir -p "${log_dir}"; then
		# If creating the log directory fails, we can't even log to the file
		# path reliably. Report this critical error immediately and exit.
		echo -e "${RED}Critical Error: Failed to create or ensure the log directory: ${log_dir}. Please check permissions and the DOTFILES_LOG_FILE variable.${RESET}" >&2
		return 1
	fi

	# --- Continue with other environment directories (now log_message can write to file) ---
	log_message INFO "Log directory ensured: \`${log_dir}\`"
	log_message INFO "Ensuring other core dotfiles environment directories..."

	# List of other directories derived from env.sh variables that must exist
	# We exclude the log directory path here as it's already handled above.
	local dirs_to_ensure=(
		"${DOTFILES_STATE_HOME}"
		"${DOTFILES_DATA_HOME}"
		"${DOTFILES_CONFIG_HOME}"
		"${DOTFILES_CACHE_HOME}"
	)

	local environment_ok=0 # Flag to track if all directories were successfully ensured

	# Iterate through the list of directories
	for dir in "${dirs_to_ensure[@]}"; do
		# Validate that the directory path variable is not empty
		if [ -z "$dir" ]; then
			log_message WARN "Found an empty path in the list of directories to ensure. Skipping empty path variable."
			# We don't set environment_ok=1 here, as an empty variable isn't a
			# creation failure of a *specific* directory, just a configuration
			# issue that might be intended.
			continue
		fi

		log_message INFO "Checking/creating directory: \`${dir}\`"

		# Attempt to create the directory. -p ensures parent directories are
		# created and doesn't fail if the directory already exists.
		if ! mkdir -p "${dir}"; then
			log_message ERROR "Failed to create or ensure directory: \`${dir}\`. Please check permissions."
			environment_ok=1 # Mark as failure but continue to check other directories
		else
			log_message INFO "Directory ensured: \`${dir}\`"
		fi
	done

	# Report the overall status based on the flag using log_message
	if [[ ${environment_ok} -eq 0 ]]; then
		log_message SUCCESS "All core dotfiles environment directories successfully ensured."
		return 0 # Success
	else
		log_message ERROR "One or more core dotfiles environment directories could not be ensured. See logs/output above."
		return 1 # Failure
	fi
}
export -f ensure_environment

# log_message LEVEL MESSAGE
# Logs a message to stdout (with colors) and appends it to the log file.
# Args:
#   $1: LEVEL - The log level (e.g., INFO, SUCCESS, ERROR, WARN).
#   $2: MESSAGE - The message string.
#
function log_message() {
	local level="$1"
	local message="$2"
	local timestamp="$(date "+%Y-%m-%d %H:%M:%S")" # ISO 8601 format suitable for logs
	local color="${RESET}"                         # Default color

	# Determine color based on level
	case "${level}" in
	SUCCESS) color="${GREEN}" ;;
	ERROR) color="${RED}" ;;
	INFO) color="${CYAN}" ;; # Using CYAN for general info
	WARN) color="${GOLD}" ;; # Using GOLD for warnings
	*) color="${GREY}" ;;    # Default for unknown levels (if needed)
	esac

	# Output to stdout with color
	# Use echo -e to interpret color escape sequences
	echo -e "${color}[${timestamp}] [${level}] ${message}${RESET}"

	# Append to log file (without color codes)
	# Use a simple timestamp format for the file
	echo "[${timestamp}] [${level}] ${message}" >>"${DOTFILES_LOG_FILE}"
}
export -f log_message

# install_symlink SOURCE TARGET
# Creates a symlink from SOURCE to TARGET.
# If TARGET exists:
# - If it's the correct symlink, does nothing and returns 0.
# - If it's a wrong symlink, removes it.
# - If it's a file or directory, backs it up to "${DOTFILES_STATE}/backups/$(basename TARGET)"
#   after ensuring the backup directory exists, and then removes TARGET.
# After handling existing TARGET, creates the symlink.
#
# Arguments:
#   $1: SOURCE - The source path for the symlink.
#   $2: TARGET - The desired path for the symlink.
#
function install_symlink() {
	local source_dir="$1"
	local target_dir="$2"
	# Define the backup root relative to DOTFILES_STATE
	local backup_root_dir="${DOTFILES_STATE_HOME}/backups"
	local target_basename="$(basename "${target_dir}")"
	# Backup path includes the basename of the target
	local backup_target_path="${backup_root_dir}/${target_basename}"

	# Validate inputs
	if [ -z "$source_dir" ] || [ -z "$target_dir" ]; then
		log_message ERROR "install_symlink requires source and target paths." >&2
		return 1
	fi
	# Ensure DOTFILES_STATE_HOME is set (basic check for backup location)
	if [ -z "${DOTFILES_STATE_HOME:-}" ]; then
		log_message ERROR "DOTFILES_STATE_HOME is not set. Cannot perform backup or log reliably." >&2
		return 1
	fi

	log_message INFO "Attempting to install symlink: \`${target_dir}\` -> \`${source_dir}\`"

	if [ -L "${target_dir}" ]; then
		rm "${target_dir}"
	fi

	# Use -e for explicit check, -L for symlink check
	if [ -e "${target_dir}" ]; then
		# If it exists, check if it's a symlink.
		if [ -L "${target_dir}" ]; then
			# If it's a symlink, check if it points to the correct directory.
			if [ "$(readlink "${target_dir}")" = "${source_dir}" ]; then
				log_message SUCCESS "Symlink already exists and points to the correct directory: \`${target_dir}\` -> \`${source_dir}\`"
				return 0 # Success, nothing needed
			else
				# If it's a symlink but points to the wrong directory, remove it.
				log_message INFO "Removing existing symlink (wrong target): \`${target_dir}\`"
				if rm "${target_dir}"; then
					log_message SUCCESS "Removed existing incorrect symlink: \`${target_dir}\`"
				else
					log_message ERROR "Failed to remove wrong symlink: \`${target_dir}\`" >&2
					return 1
				fi
			fi
		else
			# If it's not a symlink (i.e., it's a regular directory or file), back it up.
			backup_item "${target_dir}"
			rm -r "${target_dir}"
		fi
	fi

	# At this point, TARGET_DIR should not exist or was successfully
	# removed/backed up. Create the symlink.
	log_message INFO "Creating symlink: \`${source_dir}\` -> \`${target_dir}\`"
	if ln -s "${source_dir}" "${target_dir}"; then
		log_message SUCCESS "Symlink created successfully: \`${target_dir}\`"
		return 0 # Success
	else
		log_message ERROR "Failed to create symlink: \`${source_dir}\` -> \`${target_dir}\`" >&2
		return 1 # Failure
	fi
}
export -f install_symlink

# ensure_block: Ensures a block of text exists in a file, identified by a block name.
# It can take the block content as a third argument or read it from standard input.
# If the block name exists, the old block is replaced. If not, the new block is appended.
#
# Arguments:
#   $1: The file path relative to HOME (e.g., ".zshenv").
#   $2: The name of the block (e.g., "MY_ALIASES", "PATH_SETUP").
#   $3 (optional): The block of text content to ensure is present in the file.
# Standard Input (if $3 is not provided):
#   The block of text content to ensure is present in the file.
#
# Side effects:
#   - Creates the file if it does not exist.
#   - Inserts the block with markers if the block name is not found.
#   - Replaces the block content between markers if the block name is found.
#   - Creates a backup file (.bak) when modifying the target file using sed.
#
# Returns:
#   0: The block was successfully ensured (inserted or updated).
#   1: An error occurred.
#
function blockinfile() {
	local file="$1"
	local block_name="$2"
	local block_content=""

	# Define markers based on the block name. Use comments appropriate for shell scripts.
	local start_marker="# BEGIN ${block_name}"
	local end_marker="# END ${block_name}"

	# --- Argument Parsing ---
	# Need at least 2 arguments (filename, block_name).
	# Block content comes from $3 or stdin.
	if [ "$#" -eq 3 ]; then
		# Block content is provided as the third argument.
		block_content="$3"
	elif [ "$#" -eq 2 ]; then
		# Block content is read from standard input.
		# Using read -d '' allows reading multi-line input.
		# The '|| true' prevents the script from exiting if stdin is empty.
		read -r -d '' block_content || true
	else
		# Usage message goes to stderr, not typically logged via log_message
		echo "Usage: ensure_block <filename> <block_name> [block_string]" >&2
		echo "Or:    ensure_block <filename> <block_name> < block_file" >&2
		log_message ERROR "Incorrect number of arguments provided to ensure_block."
		return 1
	fi

	# Validate block_name - simple check for empty string
	if [ -z "${block_name}" ]; then
		log_message ERROR "Block name cannot be empty."
		return 1
	fi

	# --- File Existence Check ---
	# Check if the file exists. If not, create it.
	if [ ! -f "${file}" ]; then
		log_message INFO "Creating file: \`${file}\`"
		touch "${file}" || {
			log_message ERROR "Could not create file: \`${file}\`"
			return 1
		}
	fi

	# --- Prepare the full block content with markers ---
	# Add a newline before the start marker if the file is not empty, to ensure
	# blocks are separated.
	local full_block_content=""
	if [ -s "${file}" ]; then # -s checks if file exists and is not empty
		full_block_content+="\n"
	fi
	full_block_content+="${start_marker}\n"
	full_block_content+="${block_content}\n"
	full_block_content+="${end_marker}\n"

	# --- Use a temporary file for the block content for robustness with sed ---
	# mktemp creates a unique temporary file.
	local temp_file
	temp_file=$(mktemp) || {
		log_message ERROR "Failed to create temporary file."
		return 1
	}

	# Write the full block content (including markers) to the temporary file.
	# Use printf -v to handle potential issues with echo and complex strings,
	# then echo the variable content to the file.
	printf "%b" "${full_block_content}" >"${temp_file}" || {
		log_message ERROR "Failed to write block content to temporary file: \`${temp_file}\`"
		return 1
	}

	# --- Check if the block already exists by looking for the start marker ---
	# Use grep -q for quiet output. Anchor the pattern with ^ and $ for exact line match.
	if grep -q "^${start_marker}$" "${file}"; then
		# --- Block exists: Update it ---
		log_message INFO "Updating block \`${block_name}\` in \`${file}\`"

		# Use sed to delete the old block (lines between start and end markers, inclusive).
		# -i.bak creates a backup file with a .bak extension.
		# Need to escape forward slashes in markers if they exist, though unlikely for block names.
		# Using different delimiters for sed 's' command if needed, but 'd' is simpler.
		# The pattern is /^START_MARKER$/,/^END_MARKER$/d
		# Escape potential backslashes in markers for sed pattern matching.
		local escaped_start_marker=$(echo "${start_marker}" | sed 's/[\/&]/\\&/g')
		local escaped_end_marker=$(echo "${end_marker}" | sed 's/[\/&]/\\&/g')

		if ! sed -i "/^${escaped_start_marker}$/,/^${escaped_end_marker}$/d" "${file}"; then
			log_message ERROR "Failed to delete old block \`${block_name}\` in \`${file}\` using sed."
			# Restore from backup if deletion failed? Or rely on set -e?
			# Relying on set -e for now.
			return 1
		fi

		# Append the new block content from the temporary file.
		if ! echo -e "${full_block_content}" >>"${file}"; then
			log_message ERROR "Failed to append new block \`${block_name}\` to \`${file}\` after deletion."
			return 1
		fi

		log_message SUCCESS "Block \`${block_name}\` updated successfully in \`${file}\`."

	else
		# --- Block does not exist: Insert it ---
		log_message INFO "Inserting block \`${block_name}\` into \`${file}\`"

		# Append the new block content from the temporary file.
		if ! cat "${temp_file}" >>"${file}"; then
			log_message ERROR "Failed to append block \`${block_name}\` to \`${file}\`."
			return 1
		fi

		log_message SUCCESS "Block \`${block_name}\` inserted successfully into \`${file}\`."
	fi

	return 0
}
export -f blockinfile

# append to path if it exists
function pathappend() {
	declare arg
	for arg in "$@"; do
		test -d "$arg" || continue
		PATH=${PATH//":$arg:"/:}
		PATH=${PATH/#"$arg:"/}
		PATH=${PATH/%":$arg"/}
		export PATH="${PATH:+"$PATH:"}$arg"
	done
}
export -f pathappend

# prepend to path if it exists
function pathprepend() {
	for arg in "$@"; do
		test -d "$arg" || continue
		PATH=${PATH//:"$arg:"/:}
		PATH=${PATH/#"$arg:"/}
		PATH=${PATH/%":$arg"/}
		export PATH="$arg${PATH:+":${PATH}"}"
	done
}
export -f pathprepend


