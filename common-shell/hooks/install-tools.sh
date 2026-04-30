#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

# Run a command as root. When already root (e.g. multi-arch buildx under
# qemu-user emulation, where setuid sudo is unusable on the nosuid layer),
# exec directly. Otherwise prefer passwordless sudo. Last resort: fail loudly.
run_as_root() {
	if [ "$(id -u)" -eq 0 ]; then "$@"; return; fi
	if command -v sudo >/dev/null 2>&1 && sudo -n true 2>/dev/null; then sudo "$@"; return; fi
	echo "ERROR: need root for: $*" >&2
	return 1
}

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
		run_as_root apt-get update
		# Debian/Ubuntu renames bat -> batcat and fd -> fdfind. Users can
		# symlink or alias in their rc files if they want the short names.
		run_as_root apt-get install -y bat fd-find fzf ripgrep curl
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
