#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

# Defensive guard: the manifest already scopes this hook under
# platform.darwin, but if it ever gets called on linux, exit clean rather
# than running the macOS installer.
if [ "$(uname -s)" != "Darwin" ]; then
	echo "homebrew: not on darwin; skipping."
	exit 0
fi

if command -v brew >/dev/null 2>&1; then
	echo "homebrew: already installed at $(command -v brew)."
	exit 0
fi

echo "homebrew: installing via official installer (https://brew.sh)"
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
