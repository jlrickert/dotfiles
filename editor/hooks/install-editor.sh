#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

# Run a command as root when not already root. apt + add-apt-repository
# need root; brew install does not. Defined inline so the hook does not
# depend on any framework helper from the legacy pkg/ tree.
run_as_root() {
	if [ "$(id -u)" -eq 0 ]; then
		"$@"
	elif command -v sudo >/dev/null 2>&1; then
		sudo "$@"
	else
		echo "editor: need root for: $*; install sudo or run as root." >&2
		return 1
	fi
}

# --- 1. Ensure neovim is installed ---
# darwin → brew (assumed present, hard-fail if not).
# debian/ubuntu → neovim-ppa/unstable for a recent enough nvim for kickstart.
# everything else → warn and skip; user can install manually.
if command -v nvim >/dev/null 2>&1; then
	echo "editor: nvim already installed at $(command -v nvim)."
else
	case "$(uname -s)" in
	Darwin)
		if ! command -v brew >/dev/null 2>&1; then
			echo "editor: Homebrew not found on macOS; install brew first." >&2
			exit 1
		fi
		echo "editor: installing neovim via brew"
		brew install neovim
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
			echo "editor: installing neovim via PPA on ${ID}"
			run_as_root add-apt-repository ppa:neovim-ppa/unstable -y
			run_as_root apt-get update
			run_as_root apt-get install -y neovim
			;;
		*)
			echo "editor: no automatic neovim install path on '${ID:-unknown}'; install manually." >&2
			;;
		esac
		;;
	*)
		echo "editor: unsupported OS '$(uname -s)'; install neovim manually." >&2
		;;
	esac
fi

# --- 2. Sync kickstart.nvim into ~/.config/nvim ---
# Only manage the directory if it doesn't exist or is already our fork.
# Don't clobber a user's hand-rolled config; warn and continue.
KICKSTART_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/nvim"
KICKSTART_REPO="https://github.com/jlrickert/kickstart.nvim"

if [ -d "${KICKSTART_DIR}/.git" ]; then
	if git -C "${KICKSTART_DIR}" remote get-url origin 2>/dev/null | grep -q "jlrickert/kickstart.nvim"; then
		echo "editor: refreshing kickstart.nvim in ${KICKSTART_DIR}"
		# Best-effort pull. Don't tank the install on a dirty tree or
		# offline machine.
		git -C "${KICKSTART_DIR}" pull --ff-only || echo "editor: kickstart.nvim pull failed; leaving alone."
	else
		echo "editor: ${KICKSTART_DIR} is a git repo but not jlrickert/kickstart.nvim; leaving alone."
	fi
elif [ ! -e "${KICKSTART_DIR}" ]; then
	echo "editor: cloning ${KICKSTART_REPO} into ${KICKSTART_DIR}"
	mkdir -p "$(dirname "${KICKSTART_DIR}")"
	git clone "${KICKSTART_REPO}" "${KICKSTART_DIR}"
else
	echo "editor: ${KICKSTART_DIR} exists but is not a git checkout; leaving alone."
fi

# --- 3. cmd-edit (jlrickert/formulae/edit) ---
# Provided as a brew formula. Linux release pipeline is broken upstream;
# warn and skip rather than fail so the rest of the install completes.
case "$(uname -s)" in
Darwin)
	if command -v edit >/dev/null 2>&1; then
		echo "editor: cmd-edit already installed at $(command -v edit)."
	elif ! command -v brew >/dev/null 2>&1; then
		echo "editor: brew missing; skipping cmd-edit." >&2
	else
		echo "editor: installing jlrickert/formulae/edit via brew"
		brew install jlrickert/formulae/edit
	fi
	;;
Linux)
	echo "editor: cmd-edit linux release pipeline is currently broken; skipping."
	;;
*)
	echo "editor: cmd-edit unsupported on '$(uname -s)'; skipping."
	;;
esac
