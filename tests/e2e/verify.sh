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
# Phase-3 dev/889 migrations: claude/, codex/, editor/, knut/, rust/,
# homebrew/, bun/. The Ubuntu test images don't install these packages
# yet (see docker/ubuntu/Dockerfile), so we only assert the manifests
# exist in the source tree — proof the migration landed without trying
# to verify runtime side effects. Add runtime assertions here once the
# image installs them.
DOTFILES_SRC="${DOTFILES_SRC:-/opt/dotfiles-src}"
for pkg in claude codex editor knut rust homebrew bun python clone; do
	assert_file "${DOTFILES_SRC}/${pkg}/Dotfile.yaml"
done
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
assert_cmd jq
assert_cmd yq
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
	# python package: python3 (>=3.11 floor) and pip3 on PATH. The package
	# has no user config to link, so only runtime assertions are exercised.
	assert_cmd python3
	if python3 -c 'import sys; assert sys.version_info >= (3, 11)' >/dev/null 2>&1; then
		pass "python3 >= 3.11 ($(python3 -c 'import sys; print(".".join(map(str, sys.version_info[:3])))'))"
	else
		fail "python3 below 3.11 floor"
	fi
	assert_cmd pip3
	assert_cmd uv

	# clone package ships gh-clone / bb-clone (Python). Depends on python
	# (uv, python3 >= 3.11), so the runtime asserts live in the full profile
	# block. The shared library _clone_lib.py is imported by both
	# entry-points, so we don't assert it as a command -- only the
	# entry-points are exec'd.
	assert_cmd gh-clone
	assert_cmd bb-clone
	# Legacy cleanup: clone/hooks/install-clone.sh removes the pre-dots
	# `clone` helper.
	if [ ! -e "${HOME}/.local/state/dotfiles/pkg/00_shell/bin/clone" ]; then
		pass "legacy clone helper absent"
	else
		fail "legacy clone helper still present at ${HOME}/.local/state/dotfiles/pkg/00_shell/bin/clone"
	fi

	# Exercise the post-clone gate end-to-end with a local fixture. Using
	# file:// keeps this hermetic — no network, no real GitHub. _clone_lib
	# treats file:// URLs as owner='local'.
	CLONE_TEST_TMP="$(mktemp -d)"
	export REPOS="${CLONE_TEST_TMP}/repos"
	mkdir -p "${REPOS}"

	# Clean fixture: empty git repo with a single README.
	CLEAN_SRC="${CLONE_TEST_TMP}/clean-repo"
	mkdir -p "${CLEAN_SRC}"
	(
		cd "${CLEAN_SRC}"
		git init -q
		git config user.email "verify@example.invalid"
		git config user.name "verify"
		echo "clean" >README
		git add README
		git commit -q -m "init"
	)
	if gh-clone "file://${CLEAN_SRC}" >/dev/null 2>&1; then
		pass "gh-clone succeeded on clean fixture"
	else
		fail "gh-clone failed on clean fixture"
	fi
	assert_file "${REPOS}/github.com/local/clean-repo/README"

	# Dirty fixture: same shape but with a .claude/settings.json (block-set
	# detector). Must be quarantined and gh-clone must exit non-zero.
	DIRTY_SRC="${CLONE_TEST_TMP}/dirty-repo"
	mkdir -p "${DIRTY_SRC}/.claude"
	(
		cd "${DIRTY_SRC}"
		git init -q
		git config user.email "verify@example.invalid"
		git config user.name "verify"
		echo "{}" >.claude/settings.json
		echo "dirty" >README
		git add .
		git commit -q -m "init"
	)
	if gh-clone "file://${DIRTY_SRC}" >/dev/null 2>&1; then
		fail "gh-clone exited 0 on dirty fixture (expected non-zero)"
	else
		pass "gh-clone exited non-zero on dirty fixture"
	fi
	if [ -d "${REPOS}/.untrusted/local/dirty-repo" ]; then
		pass "dirty fixture quarantined under .untrusted/local/dirty-repo"
	else
		fail "dirty fixture not at ${REPOS}/.untrusted/local/dirty-repo"
	fi
	if [ ! -e "${REPOS}/github.com/local/dirty-repo" ]; then
		pass "dirty fixture absent from canonical github.com path"
	else
		fail "dirty fixture still at canonical github.com/local/dirty-repo"
	fi
	unset REPOS
	rm -rf "${CLONE_TEST_TMP}"
fi

assert_shell_loads bash
assert_shell_loads zsh

summary
