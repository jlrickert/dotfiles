#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

BEGIN_MARKER="# BEGIN dotfiles-zsh"
END_MARKER="# END dotfiles-zsh"

ensure_zsh_installed() {
	if command -v zsh >/dev/null 2>&1; then
		echo "zsh: already installed at $(command -v zsh)"
		return 0
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
}

# Render the marker block for a given relative path under $XDG_CONFIG_HOME.
# Variables in the output are kept literal so they expand at shell-load time.
render_block() {
	local rel_path="$1"
	cat <<EOF
${BEGIN_MARKER}
[ -r "\${XDG_CONFIG_HOME:-\$HOME/.config}/${rel_path}" ] && . "\${XDG_CONFIG_HOME:-\$HOME/.config}/${rel_path}"
${END_MARKER}
EOF
}

# Inject (or refresh) the marker block in target. Preserves any other
# content -- this is what lets installer-appends from bun, fnm, conda,
# Egnyte egcli, etc. coexist with the dotfiles source line.
inject_stub() {
	local target="$1" rel_path="$2"
	local block
	block="$(render_block "${rel_path}")"

	if [ ! -f "${target}" ]; then
		printf '%s\n' "${block}" >"${target}"
		echo "zsh: created ${target} with dotfiles marker"
		return 0
	fi

	if grep -qF "${BEGIN_MARKER}" "${target}"; then
		local tmp
		tmp="$(mktemp)"
		BLOCK="${block}" awk \
			-v begin="${BEGIN_MARKER}" \
			-v end="${END_MARKER}" '
			$0 == begin { print ENVIRON["BLOCK"]; in_block=1; next }
			$0 == end && in_block { in_block=0; next }
			!in_block { print }
		' "${target}" >"${tmp}"
		mv "${tmp}" "${target}"
		echo "zsh: refreshed dotfiles marker in ${target}"
	else
		printf '\n%s\n' "${block}" >>"${target}"
		echo "zsh: appended dotfiles marker to ${target}"
	fi
}

ensure_zsh_installed
inject_stub "${HOME}/.zshenv" "zsh/zshenv"
inject_stub "${HOME}/.zshrc" "zsh/zshrc"
