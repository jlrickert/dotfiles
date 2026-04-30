#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

# Pinned Go toolchain version for the Linux tarball path. macOS uses brew
# and lets brew own the version. Bump on a separate version-bump commit.
GO_VERSION="1.24.4"

case "$(uname -s)" in
Darwin)
	if ! command -v brew >/dev/null 2>&1; then
		echo "go: Homebrew not found on macOS; install brew first." >&2
		exit 1
	fi
	if command -v go >/dev/null 2>&1; then
		echo "go: $(go version) already on PATH; skipping brew install."
		exit 0
	fi
	brew install go
	;;
Linux)
	if [ ! -r /etc/os-release ]; then
		echo "go: cannot identify Linux distribution from /etc/os-release." >&2
		exit 1
	fi
	# shellcheck disable=SC1091
	. /etc/os-release
	case "${ID:-}" in
	debian | ubuntu) ;;
	*)
		echo "go: unsupported Linux distro '${ID:-unknown}'; install from https://go.dev/dl/." >&2
		exit 0
		;;
	esac

	GO_INSTALL_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/go"
	# Defer to any go already on PATH from another source (system apt, asdf,
	# manual install). Only manage the tarball path we own. If the resolved
	# `go` is exactly $GO_INSTALL_DIR/bin/go, fall through so the
	# version-compare/replace block below can refresh stale tarballs.
	if command -v go >/dev/null 2>&1; then
		existing="$(command -v go)"
		if [ "$existing" != "$GO_INSTALL_DIR/bin/go" ]; then
			echo "go: $("$existing" version) already on PATH at $existing; skipping tarball install."
			exit 0
		fi
	fi
	if [ -x "$GO_INSTALL_DIR/bin/go" ]; then
		installed="$("$GO_INSTALL_DIR/bin/go" version 2>/dev/null | awk '{print $3}')"
		if [ "$installed" = "go${GO_VERSION}" ]; then
			echo "go: ${installed} already at $GO_INSTALL_DIR; skipping."
			exit 0
		fi
		echo "go: replacing $installed at $GO_INSTALL_DIR with go${GO_VERSION}."
		rm -rf "$GO_INSTALL_DIR"
	fi

	case "$(uname -m)" in
	x86_64) GO_ARCH=amd64 ;;
	aarch64 | arm64) GO_ARCH=arm64 ;;
	*)
		echo "go: unsupported Linux architecture '$(uname -m)'." >&2
		exit 1
		;;
	esac

	tarball="go${GO_VERSION}.linux-${GO_ARCH}.tar.gz"
	url="https://go.dev/dl/${tarball}"
	tmp="$(mktemp -d)"
	trap 'rm -rf "$tmp"' EXIT

	echo "go: downloading $url"
	curl -fsSL "$url" -o "$tmp/$tarball"

	mkdir -p "$(dirname "$GO_INSTALL_DIR")"
	tar -xzf "$tmp/$tarball" -C "$tmp"
	mv "$tmp/go" "$GO_INSTALL_DIR"
	echo "go: installed go${GO_VERSION} to $GO_INSTALL_DIR"
	;;
*)
	echo "go: unsupported OS '$(uname -s)'." >&2
	exit 1
	;;
esac
