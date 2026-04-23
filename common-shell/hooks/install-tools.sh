#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

# General CLI toolkit used by both bash and zsh configs:
#   starship (prompt), bat (cat), fd (find), fzf (fuzzy), ripgrep (grep)

install_starship_curl() {
	# starship is not reliably packaged on Debian/Ubuntu; fall back to upstream.
	if ! command -v starship >/dev/null 2>&1; then
		curl -sS https://starship.rs/install.sh | sh -s -- -y
	fi
}

case "$(uname -s)" in
Darwin)
	if ! command -v brew >/dev/null 2>&1; then
		echo "common-shell: Homebrew not found; skipping CLI toolkit." >&2
		exit 0
	fi
	brew install starship bat fd fzf ripgrep
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
		# Debian/Ubuntu renames bat -> batcat and fd -> fdfind. Users can
		# symlink or alias in their rc files if they want the short names.
		sudo apt-get install -y bat fd-find fzf ripgrep curl
		install_starship_curl
		;;
	*)
		echo "common-shell: unsupported Linux distro '${ID:-unknown}'; skipping." >&2
		;;
	esac
	;;
*)
	echo "common-shell: unsupported OS '$(uname -s)'; skipping." >&2
	;;
esac
