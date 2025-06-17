#!/usr/bin/env bash

GO_VERSION="1.24.4"
GO_OS="linux"
GO_ARCH="amd64"
GO_TARBALL="go${GO_VERSION}.${GO_OS}-${GO_ARCH}.tar.gz"
GO_URL="https://go.dev/dl/${GO_TARBALL}"
# Target installation directory
GO_INSTALL_DIR="${HOME}/.local/share/go"
GO_TEMP_DIR=$(mktemp -d)

ensure_environment

# Check if Go is already installed
if have go; then

	# Get the full Go version output (e.g., "go version go1.21.5 linux/amd64")
	# Capture stderr to avoid polluting output if 'go version' prints warnings,
	# though it's less common for 'go version'.
	GO_FULL_OUTPUT=$(go version 2>/dev/null)

	# Extract the version string (e.g., "go1.21.5")
	GO_RAW_VERSION=$(echo "${GO_FULL_OUTPUT}" | awk '{print $3}' 2>/dev/null)

	# Remove the leading "go" prefix (e.g., "1.21.5")
	GO_VERSION_NUMBERS=$(echo "$GO_RAW_VERSION" | sed 's/^go//')

	# Split the version string into major, minor, and patch
	# Using bash string manipulation for clarity if version is like "1.21.5"
	# Alternative: echo "$GO_VERSION_NUMBERS" | cut -d. -f1,2,3
	GO_MAJOR=$(echo "${GO_VERSION_NUMBERS}" | cut -d. -f1)
	GO_MINOR=$(echo "${GO_VERSION_NUMBERS}" | cut -d. -f2)
	GO_PATCH=$(echo "${GO_VERSION_NUMBERS}" | cut -d. -f3)

	# No need to continue as brew will handling updating versions
	if have brew; then
		log_message INFO "Go is already installed and managed by brew."
		exit 0
	fi

	if [ "${GO_MAJOR}.${GO_MINOR}.${GO_PATCH}" == ${GO_VERSION} ]; then
		log_message INFO "Go version ${GO_MAJOR}.${GO_MINOR}.${GO_PATCH} found. Removing for later reinstallation of newer version"
		rm -r "${XDG_DATA_HOME}/go"
	fi
else
	log_message INFO "Go not found. Attempting installation..."
fi

# Detect OS
case "$(uname -s)" in
Darwin)
	# macOS
	log_message INFO "Detected macOS. Installing Go via Homebrew..."
	# Check if Homebrew is installed
	if ! have brew; then
		log_message ERROR "Homebrew is not installed. Please install Homebrew first to install Go on macOS." >&2
		log_message INFO "See https://brew.sh/ for installation instructions."
		exit 1
	fi

	# Install Go package
	log_message INFO "Running 'brew install go'..."
	if brew install go; then
		log_message SUCCESS "Go installed successfully via Homebrew."
		exit 0
	else
		log_message ERROR "Failed to install Go via Homebrew." >&2
		exit 1
	fi
	;;

Linux)
	# Check if it's Ubuntu/Debian based
	if [ ! -f /etc/os-release ]; then
		log_message ERROR "Detected Linux but could not determine distribution from /etc/os-release. Automatic Go installation is not supported." >&2
		log_message INFO "Please refer to the official Go documentation for manual installation instructions."
		exit 1
	fi

	# Source os-release to get ID
	source /etc/os-release

	# Use bouncer pattern: If not Ubuntu/Debian, exit early
	if [[ "${ID}" != "ubuntu" && "${ID}" != "debian" ]]; then
		log_message ERROR "Detected Linux but not Ubuntu/Debian. Automatic Go installation is not supported for your distribution (${ID})." >&2
		log_message INFO "Please refer to the official Go documentation for manual installation instructions."
		exit 1
	fi

	log_message INFO "Attempting to install Go ${GO_VERSION} to ${GO_INSTALL_DIR}"

	# Check if Go is already installed in the target directory
	if [ -d "${GO_INSTALL_DIR}/bin" ] && command -v "${GO_INSTALL_DIR}/bin/go" &>/dev/null; then
		log_message SUCCESS "Go seems to be already installed at ${GO_INSTALL_DIR}"
		# Optional: Check version
		# installed_version=$("${GO_INSTALL_DIR}/bin/go" version | awk '{print $3}')
		# if [ "go${GO_VERSION}" = "$installed_version" ]; then
		#     log_message SUCCESS "Go version ${GO_VERSION} is already installed."
		# else
		#     log_message INFO "Go found at ${GO_INSTALL_DIR}, but version is ${installed_version}. Proceeding with installation of ${GO_VERSION}."
		#     # Remove existing installation to avoid conflicts
		#     log_message INFO "Removing existing Go installation at ${GO_INSTALL_DIR}"
		#     rm -rf "${GO_INSTALL_DIR}"
		# fi
	else
		log_message INFO "Go not found at ${GO_INSTALL_DIR}. Proceeding with installation."

		# Download the Go tarball
		log_message INFO "Downloading ${GO_URL} to ${GO_TEMP_DIR}/${GO_TARBALL}"
		if ! curl -fsSL "${GO_URL}" -o "${GO_TEMP_DIR}/${GO_TARBALL}"; then
			log_message ERROR "Failed to download Go tarball." >&2
			rm -rf "${GO_TEMP_DIR}"
			exit 1
		fi

		# Create the installation directory
		log_message INFO "Creating installation directory: ${GO_INSTALL_DIR}"
		mkdir -p "${GO_INSTALL_DIR}"

		# Extract the tarball
		log_message INFO "Extracting tarball to ${GO_TEMP_DIR}"
		if ! tar -xzf "${GO_TEMP_DIR}/${GO_TARBALL}" -C "${GO_TEMP_DIR}"; then
			log_message ERROR "Failed to extract Go tarball." >&2
			rm -rf "${GO_TEMP_DIR}"
			exit 1
		fi

		# Move the extracted Go directory to the target location
		log_message INFO "Moving extracted Go to ${GO_INSTALL_DIR}"
		# The tarball extracts into a 'go' directory
		if [ -d "${GO_TEMP_DIR}/go" ]; then
			# Remove existing directory if it exists (e.g., if check above was skipped or partial install)
			if [ -d "${GO_INSTALL_DIR}" ]; then
				log_message INFO "Removing existing directory at ${GO_INSTALL_DIR} before moving."
				rm -rf "${GO_INSTALL_DIR}"
			fi
			mv "${GO_TEMP_DIR}/go" "${GO_INSTALL_DIR}"
			log_message SUCCESS "Go ${GO_VERSION} installed successfully to ${GO_INSTALL_DIR}"
		else
			log_message ERROR "Extracted 'go' directory not found in temporary location." >&2
			rm -rf "${GO_TEMP_DIR}"
			exit 1
		fi
	fi

	# Clean up temporary directory
	log_message INFO "Cleaning up temporary directory: ${GO_TEMP_DIR}"
	rm -rf "${GO_TEMP_DIR}"
	;;

*)
	log_message ERROR "Unsupported operating system: $(uname -s). Cannot automatically install Go." >&2
	log_message INFO "Please refer to the official Go documentation for manual installation instructions."
	exit 1
	;;
esac
