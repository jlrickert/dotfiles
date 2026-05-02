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
#   starship (prompt), bat (cat), fd (find), fzf (fuzzy), ripgrep (grep),
#   jq (JSON), yq (YAML; mikefarah/yq Go binary, not the Python wrapper)

install_starship_curl() {
	# starship is not reliably packaged on Debian/Ubuntu; fall back to upstream.
	if ! command -v starship >/dev/null 2>&1; then
		curl -sS https://starship.rs/install.sh | sh -s -- -y
	fi
}

install_yq_curl() {
	# mikefarah/yq (Go) is not in apt — the Debian/Ubuntu 'yq' package is
	# kislyuk/yq (Python). Pull the upstream binary release directly.
	if command -v yq >/dev/null 2>&1; then return; fi
	local arch
	case "$(uname -m)" in
	x86_64) arch=amd64 ;;
	aarch64 | arm64) arch=arm64 ;;
	*)
		echo "common-shell: unsupported arch '$(uname -m)' for yq; skipping." >&2
		return 0
		;;
	esac
	local tmp
	tmp="$(mktemp)"
	curl -fsSL "https://github.com/mikefarah/yq/releases/latest/download/yq_linux_${arch}" -o "${tmp}"
	run_as_root install -m 0755 "${tmp}" /usr/local/bin/yq
	rm -f "${tmp}"
}

case "$(uname -s)" in
Darwin)
	if ! command -v brew >/dev/null 2>&1; then
		echo "common-shell: Homebrew not found; skipping CLI toolkit." >&2
		exit 0
	fi
	brew install starship bat fd fzf ripgrep jq yq
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
		run_as_root apt-get install -y bat fd-find fzf ripgrep curl jq
		install_starship_curl
		install_yq_curl
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
