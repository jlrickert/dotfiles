#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

if command -v bun >/dev/null 2>&1; then
	echo "bun: already installed at $(command -v bun)."
	exit 0
fi

case "$(uname -s)" in
Darwin)
	if ! command -v brew >/dev/null 2>&1; then
		echo "bun: Homebrew not found on macOS; install brew first." >&2
		exit 1
	fi
	echo "bun: installing via brew install bun"
	brew install bun
	;;
Linux)
	# Official Bun installer drops the binary at $HOME/.bun/bin/bun.
	# profile.d/bun.sh adds that to PATH on next shell load.
	echo "bun: installing via https://bun.sh/install"
	curl -fsSL https://bun.sh/install | bash
	;;
*)
	echo "bun: unsupported OS '$(uname -s)'; install manually." >&2
	exit 0
	;;
esac
