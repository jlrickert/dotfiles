#!/usr/bin/env bash

GO_VERSION="1.24.3"
GO_OS="linux"
GO_ARCH="amd64"
GO_TARBALL="go${GO_VERSION}.${GO_OS}-${GO_ARCH}.tar.gz"
GO_URL="https://go.dev/dl/${GO_TARBALL}"
GO_EXPECTED_HASH="3333f6ea53afa971e9078895eaa4ac7204a8c6b5c68c10e6bc9a33e8e391bdd8"
# Target installation directory
GO_INSTALL_DIR="${HOME}/.local/go"
GO_TEMP_DIR=$(mktemp -d)

ensure_environment

# Check if Go is already installed
if command -v go &>/dev/null; then
	log_message INFO "Go is already installed."
	exit 0 # Exit successfully if already present
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
	. /etc/os-release

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

		# Verify the hash
		log_message INFO "Verifying checksum..."
		GO_DOWNLOADED_HASH=$(sha256sum "${GO_TEMP_DIR}/${GO_TARBALL}" | awk '{print $1}')
		if [ "${GO_DOWNLOADED_HASH}" != "${GO_EXPECTED_HASH}" ]; then
			log_message ERROR "Checksum mismatch! Expected ${GO_EXPECTED_HASH}, got ${GO_DOWNLOADED_HASH}" >&2
			rm -rf "${GO_TEMP_DIR}"
			exit 1
		fi
		log_message SUCCESS "Checksum verified."

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

	# Note: Setting GOPATH and adding ${GO_INSTALL_DIR}/bin to PATH should be handled
	# by your shell's environment setup (e.g., in .bashrc, .zshrc, or a sourced env.sh file).
	# This script only handles the installation of the Go distribution files.

	;;

*)
	log_message ERROR "Unsupported operating system: $(uname -s). Cannot automatically install Go." >&2
	log_message INFO "Please refer to the official Go documentation for manual installation instructions."
	exit 1
	;;
esac
