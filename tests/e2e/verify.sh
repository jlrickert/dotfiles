#!/usr/bin/env bash
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=lib.sh
. "${HERE}/lib.sh"

cd "${HOME}"

# VERIFY_PROFILE selects which package set was installed. "slim" (default)
# = common-shell + bash + zsh; "full" = slim + zellij + go. Assertions for
# packages outside the active profile are skipped.
VERIFY_PROFILE="${VERIFY_PROFILE:-slim}"

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
if [ "${VERIFY_PROFILE}" = full ]; then
	assert_file "${HOME}/.config/zellij/config.kdl"
	assert_cmd zellij
	assert_cmd mux
	assert_cmd set-zellij-colorscheme
fi
# Real zsh configs live under ~/.config/zsh/.
assert_file "${HOME}/.config/zsh/zshenv"
assert_file "${HOME}/.config/zsh/zshrc"
assert_file "${HOME}/.config/zsh/lib/termsupport.zsh"
assert_file "${HOME}/.config/zsh/lib/zsh-vi-mode.zsh"
# Per-tool completions are installed by their packages into shared dirs that
# bash and zsh discover automatically. mux ships with the zellij package.
if [ "${VERIFY_PROFILE}" = full ]; then
	assert_file "${HOME}/.config/zsh/completions/_mux"
	assert_file "${HOME}/.config/bash/completions/mux.bash"
fi
# dots completions are installed by the bash and zsh packages themselves
# (slim profile + every superset). Sanity-check the head sentinel to catch
# truncated/empty writes -- cobra V2 bash output starts with a "# bash
# completion V2" banner; zsh output starts with "#compdef dots".
assert_file "${HOME}/.config/bash/completions/dots.bash"
assert_file "${HOME}/.config/zsh/completions/_dots"
assert_grep "${HOME}/.config/bash/completions/dots.bash" "bash completion V2 for dots"
assert_grep "${HOME}/.config/zsh/completions/_dots" "#compdef dots"
# ~/.zshrc and ~/.zshenv are NOT package files -- the install hook injects
# a marker block that sources ~/.config/zsh/. Verify the block is present.
assert_grep "${HOME}/.zshrc" "BEGIN dotfiles-zsh"
assert_grep "${HOME}/.zshenv" "BEGIN dotfiles-zsh"
# History reliability under terminal multiplexers: per-command writes via
# inc_append_history_time, with SAVEHIST matching HISTSIZE so saves aren't
# silently truncated. The mkdir in zshrc must create the HISTFILE parent.
assert_grep "${HOME}/.config/zsh/zshrc" "inc_append_history"
assert_grep "${HOME}/.config/zsh/zshrc" "SAVEHIST=50000"
zsh -i -c 'echo ok' >/dev/null 2>&1 || true
if [ -d "${HOME}/.local/state/zsh" ]; then
	pass "${HOME}/.local/state/zsh exists (HISTFILE parent)"
else
	fail "${HOME}/.local/state/zsh missing (HISTFILE parent not created)"
fi

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
# go package: toolchain on PATH (via profile.d/go.sh), static go env config.
if [ "${VERIFY_PROFILE}" = full ]; then
	assert_cmd go
	assert_file "${HOME}/.config/go/env"
	assert_file "${HOME}/.config/dots/profile.d/go.sh"
fi

assert_shell_loads bash
assert_shell_loads zsh

summary
