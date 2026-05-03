#!/usr/bin/env bash
set -euo pipefail

# Three runtimes share this hook: bun, fnm-managed node, and deno.
# Each runtime is gated by `command -v <bin>` early-skip and isolated in its
# own numbered section so a partial failure (e.g., fnm CDN down) leaves a
# diagnosable trail rather than fusing all three into one step. Network
# installers are wrapped with `|| true` so a CDN hiccup doesn't abort the
# whole package — the post-install re-check then logs a wiring warning if
# the binary still isn't visible. We don't fail the hook on missing binaries
# because tests/e2e/verify.sh asserts them; failing here would mask the real
# failure surface.

OS="$(uname -s)"

# --- 1. bun ---
# darwin → brew bun. linux → official curl installer at https://bun.sh/install
# (drops the binary at $HOME/.bun/bin/bun; profile.d/bun.sh adds it to PATH).
if command -v bun >/dev/null 2>&1; then
	echo "javascript: bun already installed at $(command -v bun); skipping."
else
	case "${OS}" in
	Darwin)
		if ! command -v brew >/dev/null 2>&1; then
			echo "javascript: Homebrew not found on macOS; skipping bun." >&2
		else
			echo "javascript: installing bun via brew install bun"
			brew install bun || true
		fi
		;;
	Linux)
		echo "javascript: installing bun via https://bun.sh/install"
		curl -fsSL https://bun.sh/install | bash || true
		;;
	*)
		echo "javascript: unsupported OS '${OS}' for bun; skipping." >&2
		;;
	esac
	# Re-check: if PATH-discovery is wrong (installer succeeded but binary
	# isn't on the post-install PATH), warn loudly. verify.sh will assert
	# the actual command later; failing here would mask that signal.
	if ! command -v bun >/dev/null 2>&1 && [ ! -x "$HOME/.bun/bin/bun" ]; then
		echo "javascript: warning -- bun still not visible after install attempt." >&2
	fi
fi

# --- 2. fnm (Fast Node Manager) ---
# darwin → brew fnm. linux → upstream installer pinned to
# $HOME/.local/share/fnm with --skip-shell so we don't double up on the
# shellenv eval — profile.d/node.sh handles that. fnm itself manages node
# versions; we don't install node directly. `fnm install --lts` could be
# added later if a default version is desired.
if command -v fnm >/dev/null 2>&1; then
	echo "javascript: fnm already installed at $(command -v fnm); skipping."
else
	case "${OS}" in
	Darwin)
		if ! command -v brew >/dev/null 2>&1; then
			echo "javascript: Homebrew not found on macOS; skipping fnm." >&2
		else
			echo "javascript: installing fnm via brew install fnm"
			brew install fnm || true
		fi
		;;
	Linux)
		echo "javascript: installing fnm via https://fnm.vercel.app/install"
		curl -fsSL https://fnm.vercel.app/install | bash -s -- \
			--install-dir "$HOME/.local/share/fnm" --skip-shell || true
		;;
	*)
		echo "javascript: unsupported OS '${OS}' for fnm; skipping." >&2
		;;
	esac
	if ! command -v fnm >/dev/null 2>&1 && [ ! -x "$HOME/.local/share/fnm/fnm" ]; then
		echo "javascript: warning -- fnm still not visible after install attempt." >&2
	fi
fi

# --- 3. deno ---
# darwin → brew deno. linux → upstream installer (drops binary at
# $HOME/.deno/bin/deno; profile.d/deno.sh adds it to PATH).
if command -v deno >/dev/null 2>&1; then
	echo "javascript: deno already installed at $(command -v deno); skipping."
else
	case "${OS}" in
	Darwin)
		if ! command -v brew >/dev/null 2>&1; then
			echo "javascript: Homebrew not found on macOS; skipping deno." >&2
		else
			echo "javascript: installing deno via brew install deno"
			brew install deno || true
		fi
		;;
	Linux)
		echo "javascript: installing deno via https://deno.land/install.sh"
		curl -fsSL https://deno.land/install.sh | sh || true
		;;
	*)
		echo "javascript: unsupported OS '${OS}' for deno; skipping." >&2
		;;
	esac
	if ! command -v deno >/dev/null 2>&1 && [ ! -x "$HOME/.deno/bin/deno" ]; then
		echo "javascript: warning -- deno still not visible after install attempt." >&2
	fi
fi
