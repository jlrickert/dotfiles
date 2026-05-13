#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

# Defensive guard: the manifest already scopes this hook under
# platform.darwin, but if it ever gets called on linux, exit clean rather
# than printing macOS-specific hints. Mirrors homebrew/hooks/install-homebrew.sh.
if [ "$(uname -s)" != "Darwin" ]; then
	exit 0
fi

# Hint-only: this package never auto-installs Podman or runs `podman
# machine init` (interactive VM sizing). If the bundled toolchain isn't
# present, point the user at the supported install path.
if [ ! -x "/opt/podman/bin/podman" ]; then
	echo "podman: /opt/podman/bin/podman not found. Install Podman Desktop from https://podman-desktop.io (the .dmg ships the full bundled toolchain)."
fi

# podman-compose is the external provider that `docker compose ...` is
# rewritten to by the podman-lab shims at ~/.local/bin/{docker,docker-compose}
# and that `podman compose` looks up on PATH. The Podman Desktop .dmg bundle
# does NOT ship it. brew install is forbidden because the formula has
# `podman` as a required dependency, which would conflict with the .dmg's
# /opt/podman/bin/podman. Use uv (preferred, modern Python tooling) or
# pipx (fallback) to get a single isolated install at ~/.local/bin/.
install_podman_compose() {
	if command -v podman-compose >/dev/null 2>&1; then
		return 0
	fi
	if command -v uv >/dev/null 2>&1; then
		uv tool install podman-compose
		return $?
	fi
	if command -v pipx >/dev/null 2>&1; then
		pipx install podman-compose
		return $?
	fi
	echo "podman: neither uv nor pipx found; install podman-compose manually: 'uv tool install podman-compose'" >&2
	return 0
}

install_podman_compose

# Generate shell completions into the shared completions dirs that
# bash/bashrc and zsh/zshrc fpath already discover. Mirrors the
# install pattern used by the bash and zsh packages for `dots`.
# Idempotent: cobra output is deterministic per podman version.
install_podman_completions() {
	# Source profile.d/podman.sh so /opt/podman/bin is on PATH for this
	# hook's subshell even when the user's outer shell hasn't sourced it yet.
	if [ -f "/opt/podman/bin/podman" ] && ! command -v podman >/dev/null 2>&1; then
		PATH="/opt/podman/bin:$PATH"
	fi

	if ! command -v podman >/dev/null 2>&1; then
		echo "podman: not on PATH; skipping completion install" >&2
		return 0
	fi

	local bash_target="${XDG_CONFIG_HOME:-$HOME/.config}/bash/completions/podman.bash"
	local zsh_target="${XDG_CONFIG_HOME:-$HOME/.config}/zsh/completions/_podman"

	mkdir -p "$(dirname "$bash_target")" "$(dirname "$zsh_target")"
	podman completion bash >"$bash_target"
	podman completion zsh >"$zsh_target"
}

install_podman_completions
