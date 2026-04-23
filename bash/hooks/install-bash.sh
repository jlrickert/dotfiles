#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

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
			sudo apt-get update
			sudo apt-get install -y bash
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
