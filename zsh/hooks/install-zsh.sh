#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

if command -v zsh >/dev/null 2>&1; then
	echo "zsh: already installed at $(command -v zsh)"
	exit 0
fi

case "$(uname -s)" in
Darwin)
	if command -v brew >/dev/null 2>&1; then
		brew install zsh
	else
		echo "zsh: Homebrew not found; install Homebrew or zsh manually." >&2
		exit 1
	fi
	;;
Linux)
	if [ -r /etc/os-release ]; then
		# shellcheck disable=SC1091
		. /etc/os-release
	else
		ID=""
	fi
	case "${ID:-}" in
	debian | ubuntu)
		sudo apt-get update
		sudo apt-get install -y zsh
		;;
	*)
		echo "zsh: unsupported Linux distro '${ID:-unknown}'; install manually." >&2
		exit 1
		;;
	esac
	;;
*)
	echo "zsh: unsupported OS '$(uname -s)'; install manually." >&2
	exit 1
	;;
esac
