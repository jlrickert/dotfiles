if ! command -v brew &>/dev/null; then
	log_message INFO "Homebrew not found. Installing..."
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
else
	log_message INFO "Homebrew is already installed."
fi
