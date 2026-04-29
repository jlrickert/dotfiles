#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

ZELLIJ_VERSION="${ZELLIJ_VERSION:-0.44.1}"

install_linux() {
	local arch_raw arch
	arch_raw="$(uname -m)"
	case "${arch_raw}" in
	x86_64 | amd64) arch="x86_64" ;;
	aarch64 | arm64) arch="aarch64" ;;
	*)
		echo "zellij: unsupported architecture '${arch_raw}'" >&2
		exit 1
		;;
	esac

	local url tmp
	url="https://github.com/zellij-org/zellij/releases/download/v${ZELLIJ_VERSION}/zellij-${arch}-unknown-linux-musl.tar.gz"
	tmp="$(mktemp -d)"
	trap 'rm -rf "${tmp}"' EXIT

	echo "zellij: downloading ${url}"
	curl -fsSL "${url}" -o "${tmp}/zellij.tar.gz"
	tar -xzf "${tmp}/zellij.tar.gz" -C "${tmp}"

	mkdir -p "${HOME}/.local/bin"
	install -m 0755 "${tmp}/zellij" "${HOME}/.local/bin/zellij"
}

ensure_zellij_installed() {
	if command -v zellij >/dev/null 2>&1; then
		echo "zellij: already installed at $(command -v zellij)"
		return 0
	fi

	case "$(uname -s)" in
	Darwin)
		if command -v brew >/dev/null 2>&1; then
			brew install zellij
		else
			echo "zellij: Homebrew not found; install Homebrew or zellij manually." >&2
			exit 1
		fi
		;;
	Linux)
		install_linux
		;;
	*)
		echo "zellij: unsupported OS '$(uname -s)'; install manually." >&2
		exit 1
		;;
	esac
}

ensure_zellij_installed
