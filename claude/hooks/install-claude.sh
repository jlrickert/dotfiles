#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

# Bail early if claude is already on PATH; the package never owns the
# binary directly. Other channels (manual install, brew on darwin, future
# linux distribution) are deferred to.
if command -v claude >/dev/null 2>&1; then
	echo "claude: already installed at $(command -v claude); skipping."
	exit 0
fi

case "$(uname -s)" in
Darwin)
	if ! command -v brew >/dev/null 2>&1; then
		echo "claude: Homebrew not found on macOS; install brew first." >&2
		exit 1
	fi
	echo "claude: installing via brew install claude-code"
	brew install claude-code
	;;
Linux)
	# Anthropic does not ship an apt/yum channel yet. Brew on Linux works
	# but is not assumed present in container images. Warn and skip rather
	# than error so unattended installs of the rest of the dotfiles tree
	# stay green.
	echo "claude: no automatic install path on Linux yet; install Claude Code manually." >&2
	;;
*)
	echo "claude: unsupported OS '$(uname -s)'; install manually." >&2
	;;
esac
