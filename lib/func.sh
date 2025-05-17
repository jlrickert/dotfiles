PROJECT_ROOT="$(cd "${SCRIPT_DIR}" && git rev-parse --show-toplevel)"
. "${PROJECT_ROOT}/lib/env.sh"

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
	log_message INFO "Log directory ensured: ${log_dir}"
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

		log_message INFO "Checking/creating directory: ${dir}"

		# Attempt to create the directory. -p ensures parent directories are
		# created and doesn't fail if the directory already exists.
		if ! mkdir -p "${dir}"; then
			log_message ERROR "Failed to create or ensure directory: ${dir}. Please check permissions."
			environment_ok=1 # Mark as failure but continue to check other directories
		else
			log_message INFO "Directory ensured: ${dir}"
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

	log_message INFO "Attempting to install symlink: ${target_dir} -> ${source_dir}"

	if [ -L "${target_dir}" ]; then
		rm "${target_dir}"
	fi

	# Use -e for explicit check, -L for symlink check
	if [ -e "${target_dir}" ]; then
		# If it exists, check if it's a symlink.
		if [ -L "${target_dir}" ]; then
			# If it's a symlink, check if it points to the correct directory.
			if [ "$(readlink "${target_dir}")" = "${source_dir}" ]; then
				log_message SUCCESS "Symlink already exists and points to the correct directory: ${target_dir} -> ${source_dir}"
				return 0 # Success, nothing needed
			else
				# If it's a symlink but points to the wrong directory, remove it.
				log_message INFO "Removing existing symlink (wrong target): ${target_dir}"
				if rm "${target_dir}"; then
					log_message SUCCESS "Removed existing incorrect symlink: ${target_dir}"
				else
					log_message ERROR "Failed to remove wrong symlink: ${target_dir}" >&2
					return 1
				fi
			fi
		else
			# If it's not a symlink (i.e., it's a regular directory or file), back it up.
			log_message INFO "Backing up existing item: ${target_dir} to ${backup_target_path}"
			# Ensure backup directory exists
			if mkdir -p "${backup_root_dir}"; then
				log_message INFO "Ensured backup directory exists: ${backup_root_dir}"
			else
				log_message ERROR "Failed to create backup directory: ${backup_root_dir}" >&2
				return 1
			fi
			if mv "${target_dir}" "${backup_target_path}"; then
				log_message SUCCESS "Backed up existing item: ${target_dir}"
			else
				log_message ERROR "Failed to backup existing item: ${target_dir}" >&2
				# Given set -o errexit, this will exit if mv fails
				return 1
			fi
		fi
	fi

	# At this point, TARGET_DIR should not exist or was successfully
	# removed/backed up. Create the symlink.
	log_message INFO "Creating symlink: ${source_dir} -> ${target_dir}"
	if ln -s "${source_dir}" "${target_dir}"; then
		log_message SUCCESS "Symlink created successfully: ${target_dir}"
		return 0 # Success
	else
		log_message ERROR "Failed to create symlink: ${source_dir} -> ${target_dir}" >&2
		return 1 # Failure
	fi
}

# ensure_block: Ensures a block of text exists in a file. It can take the block
# as a second argument or read it from standard input.
# Arguments:
#   $1: The file path relative to HOME (e.g., ".zshenv").
#   $2 (optional): The block of text to ensure is present in the file.
# Standard Input (if $2 is not provided):
#   The block of text to ensure is present in the file.
# Side effects:
#   - Creates the file if it does not exist.
#   - Appends the block to the file if it is not found.
ensure_block() {
	local file="$1"
	local block=""

	# Check number of arguments to determine source of the block.
	if [ "$#" -eq 2 ]; then
		# Block is provided as the second argument.
		block="$2"
	elif [ "$#" -eq 1 ]; then
		# Block is read from standard input.
		# Using read -d '' allows reading multi-line input.
		# The '|| true' prevents the script from exiting if stdin is empty.
		read -r -d '' block || true
	else
		# Usage message goes to stderr, not typically logged via log_message
		echo "Usage: ensure_block <filename> [block_string]" >&2
		echo "Or:    ensure_block <filename> < block_file" >&2
		return 1
	fi

	# Check if the file exists. If not, create it.
	if [ ! -f "${file}" ]; then
		log_message INFO "Creating ${file}"
		touch "${file}" || {
			log_message ERROR "Could not create ${file}"
			return 1
		}
	fi

	# Check if the block already exists in the file. Pipe the block content to
	# grep, using -F for fixed string matching, -q for quiet output, and '-' to
	# read pattern from stdin.
	if ! echo "${block}" | grep -Fq - "${file}"; then
		log_message INFO "Adding block to ${file}"
		# Append the block to the file.
		# Using echo adds a trailing newline, which is usually desired for file blocks.
		echo "${block}" >>"${file}" || {
			log_message ERROR "Could not write to ${file}"
			return 1
		}
	else
		log_message INFO "Block already exists in ${file}"
	fi
}
