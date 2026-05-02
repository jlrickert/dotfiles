#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

ensure_wezterm_installed() {
	if command -v wezterm >/dev/null 2>&1; then
		echo "wezterm: already installed at $(command -v wezterm)"
		return 0
	fi

	case "$(uname -s)" in
	Darwin)
		if command -v brew >/dev/null 2>&1; then
			# wezterm is distributed as a cask, not a formula.
			brew install --cask wezterm
		else
			echo "wezterm: Homebrew not found; install Homebrew or wezterm manually." >&2
			exit 1
		fi
		;;
	Linux)
		echo "wezterm: GUI terminal -- install via your distro package manager" >&2
		echo "         or from https://github.com/wez/wezterm/releases" >&2
		echo "wezterm: skipping binary install; config has been laid down." >&2
		;;
	*)
		echo "wezterm: unsupported OS '$(uname -s)'; install manually." >&2
		;;
	esac
}

ensure_wezterm_installed
