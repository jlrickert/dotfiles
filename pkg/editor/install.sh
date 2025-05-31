#!/usr/bin/env bash
# Installs nvim configuration by symlink

SOURCE_DIR="${PACKAGE_ROOT}/src"
TARGET_DIR="${HOME}/.config/nvim"

# Use the reusable install_symlink function
install_symlink "${SOURCE_DIR}" "${TARGET_DIR}"

# Check if Rust (cargo) is installed, as stylua is installed via cargo
if ! command -v cargo &>/dev/null; then
	log_message INFO "Rust (cargo) is not installed. Skipping stylua installation."
	# Skip the rest of this block if cargo is not available
else
	# Cargo is installed, now check for stylua
	if ! command -v stylua &>/dev/null; then
		log_message INFO "stylua not found. Installing via cargo..."
		# Use the _sudo helper if necessary, though cargo install usually installs to user's home
		# _sudo cargo install stylua --features luajit
		cargo install stylua --features luajit
		# Check if installation was successful
		if [ $? -eq 0 ]; then
			log_message SUCCESS "stylua installed successfully."
		else
			log_message ERROR "Failed to install stylua via cargo." >&2
			# Decide whether to exit or continue if stylua installation fails
			# exit 1
		fi
	else
		log_message INFO "stylua is already installed."
	fi
fi

log_message INFO "Neovim symlink installation script finished."

_sudo() {
	# use sudo if not root
	if [ "$EUID" -ne 0 ]; then
		command sudo "$@"
	else
		"$@"
	fi
}

# Check the operating system
OS=$(uname -s)

case "$OS" in
Linux)
	# Check if it's Ubuntu (or Debian-based with apt)
	if command -v apt-get &>/dev/null; then
		log_message INFO "Detected Ubuntu/Debian. Installing Neovim via PPA."
		# Add the Neovim PPA to get the latest version.
		# This is the recommended way to install Neovim on Ubuntu.
		_sudo add-apt-repository ppa:neovim-ppa/unstable -y && _sudo apt-get update
		_sudo apt-get install -y neovim
	else
		log_message ERROR "Detected Linux, but not a supported distribution for automatic Neovim installation (requires apt-get)." >&2
		log_message INFO "Please install Neovim manually."
		exit 1 # Or continue, depending on desired behavior
	fi
	;;
Darwin)
	log_message INFO "Detected macOS. Installing Neovim via Homebrew."
	# Check if brew is installed
	if ! command -v brew &>/dev/null; then
		log_message ERROR "Homebrew not found. Please install Homebrew first: https://brew.sh/" >&2
		exit 1 # Or continue
	fi
	# brew install does not typically require sudo
	brew install neovim
	;;
*)
	log_message ERROR "Unsupported operating system: ${OS}. Cannot automatically install Neovim." >&2
	log_message INFO "Please install Neovim manually."
	exit 1 # Or continue
	;;
esac
