#!/usr/bin/env bash
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=lib.sh
. "${HERE}/lib.sh"

cd "${HOME}"

# Source the shared profile so package-contributed PATH bits are visible
# (~/.local/bin, ~/.local/share/go/bin via profile.d/go.sh, brew/bun, etc.).
# This also exercises the profile.d drop-dir loop.
# shellcheck source=/dev/null
[ -r "${HOME}/.profile" ] && . "${HOME}/.profile"

# link_strategy=copy (set in dots-config/config.yaml), so installed
# dotfiles land as regular files.
assert_file "${HOME}/.bashrc"
assert_file "${HOME}/.bash_profile"
assert_file "${HOME}/.profile"
assert_file "${HOME}/.config/dots/config.yaml"
assert_file "${HOME}/.config/starship.toml"
assert_file "${HOME}/.config/zellij/config.kdl"
assert_cmd zellij
assert_cmd mux
assert_cmd set-zellij-colorscheme
# Real zsh configs live under ~/.config/zsh/.
assert_file "${HOME}/.config/zsh/zshenv"
assert_file "${HOME}/.config/zsh/zshrc"
assert_file "${HOME}/.config/zsh/lib/termsupport.zsh"
assert_file "${HOME}/.config/zsh/lib/zsh-vi-mode.zsh"
# Per-tool completions are installed by their packages into shared dirs that
# bash and zsh discover automatically.
assert_file "${HOME}/.config/zsh/completions/_mux"
assert_file "${HOME}/.config/bash/completions/mux.bash"
# ~/.zshrc and ~/.zshenv are NOT package files -- the install hook injects
# a marker block that sources ~/.config/zsh/. Verify the block is present.
assert_grep "${HOME}/.zshrc" "BEGIN dotfiles-zsh"
assert_grep "${HOME}/.zshenv" "BEGIN dotfiles-zsh"

assert_cmd starship
assert_cmd fzf
assert_cmd rg
# common-shell exports EDITOR / VISUAL from a preference chain (nvim, vim,
# nano). The Ubuntu image installs vim, so EDITOR should resolve to 'vim'.
if [ -n "${EDITOR:-}" ]; then
	pass "EDITOR set to $EDITOR"
else
	fail "EDITOR not set"
fi
if [ -n "${VISUAL:-}" ]; then
	pass "VISUAL set to $VISUAL"
else
	fail "VISUAL not set"
fi
# Debian/Ubuntu rename these binaries; the install hook installs them under
# their distro-renamed names.
assert_cmd batcat
assert_cmd fdfind

assert_shell_loads bash
assert_shell_loads zsh

summary
