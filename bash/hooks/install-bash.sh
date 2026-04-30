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

if command -v bash >/dev/null 2>&1; then
	installed="$(command -v bash)"
else
	installed=""
fi

case "$(uname -s)" in
Darwin)
	if command -v brew >/dev/null 2>&1; then
		# macOS ships an ancient bash 3.2 at /bin/bash. Homebrew bash is the
		# useful one; install it if the only bash is /bin/bash.
		if [ -z "$installed" ] || [ "$installed" = "/bin/bash" ]; then
			brew install bash
		fi
		brew install bash-completion@2
	else
		echo "bash: Homebrew not found; system bash is in use." >&2
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
		if [ -z "$installed" ]; then
			run_as_root apt-get update
			run_as_root apt-get install -y bash
		fi
		;;
	*)
		echo "bash: unsupported Linux distro '${ID:-unknown}'; skipping install." >&2
		;;
	esac
	;;
*)
	echo "bash: unsupported OS '$(uname -s)'; skipping install." >&2
	;;
esac

# Generate dots completion into the shared completions dir that bash/bashrc
# already auto-discovers. Mirrors the install pattern used by the zellij
# package's mux completion. Idempotent: cobra output is deterministic per
# dots version, so unconditional overwrite is fine.
install_dots_completion() {
	if ! command -v dots >/dev/null 2>&1; then
		echo "bash: dots not on PATH; skipping completion install" >&2
		return 0
	fi
	local target="${XDG_CONFIG_HOME:-$HOME/.config}/bash/completions/dots.bash"
	mkdir -p "$(dirname "$target")"
	dots completion bash >"$target"
	echo "bash: wrote $target"
}
install_dots_completion
