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
# homebrew/, javascript/. The Ubuntu test images don't install these packages
# yet (see docker/ubuntu/Dockerfile), so we only assert the manifests
# exist in the source tree — proof the migration landed without trying
# to verify runtime side effects. Add runtime assertions here once the
# image installs them.
DOTFILES_SRC="${DOTFILES_SRC:-/opt/dotfiles-src}"
for pkg in claude codex editor knut rust homebrew javascript wezterm python clone; do
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
# Layer-1 marker pattern: ~/.profile must contain the dotfiles-profile block.
assert_grep "${HOME}/.profile" "BEGIN dotfiles-profile"
# Real Layer-1 file lives under ~/.config/dots/user/.
assert_file "${HOME}/.config/dots/user/profile"
# Layering contract: Layer-1 and Layer-2 files must NOT source another
# shell's interactive rc. The detector sweeps every Layer-1/Layer-2 file
# (~/.profile, ~/.config/dots/user/profile, and every drop-in under
# ~/.config/dots/user/profile.d/*.sh) for actual sourcing forms — `.` or
# `source` at line start (after optional whitespace) followed by something
# ending in .bashrc or .zshrc. Comment mentions and incidental string
# matches are excluded by the leading-anchor and the (\.|source) form.
LAYERING_FILES="${HOME}/.profile ${HOME}/.config/dots/user/profile"
LAYERING_HIT=0
LAYERING_HIT_FILES=""
for layering_file in ${LAYERING_FILES}; do
	[ -f "${layering_file}" ] || continue
	if grep -E '^[[:space:]]*(\.|source)[[:space:]]+.*(\.bashrc|\.zshrc)' "${layering_file}" >/dev/null 2>&1; then
		LAYERING_HIT=1
		LAYERING_HIT_FILES="${LAYERING_HIT_FILES} ${layering_file}"
	fi
done
# POSIX no-glob-match guard: if the glob matches nothing, "$1" expands to
# the literal pattern, so the [ -e "$1" ] check ensures we only iterate
# real files.
set -- "${HOME}/.config/dots/user/profile.d/"*.sh
if [ -e "$1" ]; then
	for layering_file in "$@"; do
		if grep -E '^[[:space:]]*(\.|source)[[:space:]]+.*(\.bashrc|\.zshrc)' "${layering_file}" >/dev/null 2>&1; then
			LAYERING_HIT=1
			LAYERING_HIT_FILES="${LAYERING_HIT_FILES} ${layering_file}"
		fi
	done
fi
if [ "${LAYERING_HIT}" -eq 0 ]; then
	pass "layering contract: no Layer-1/2 file sources an interactive rc"
else
	fail "layering contract violated; cross-sourcing in:${LAYERING_HIT_FILES}"
fi
# Layer-1 env must reach non-interactive bash and zsh. ${HOME}/.local/bin
# is the canonical test target — common-shell/profile prepends it.
for sh in bash zsh; do
	if "${sh}" -c 'case ":$PATH:" in *":'"${HOME}"'/.local/bin:"*) exit 0 ;; esac; exit 1'; then
		pass "${sh} non-interactive PATH contains ${HOME}/.local/bin"
	else
		fail "${sh} non-interactive PATH missing ${HOME}/.local/bin"
	fi
done
# zshenv-tier HISTFILE: must export to non-interactive zsh subshells.
if zsh -c '[ -n "${HISTFILE:-}" ]' 2>/dev/null; then
	pass "HISTFILE visible to non-interactive zsh"
else
	fail "HISTFILE not exported from zshenv"
fi
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
	assert_file "${HOME}/.config/dots/user/profile.d/go.sh"
	assert_file "${HOME}/.config/dots/user/profile.d/bun.sh"
	assert_file "${HOME}/.config/dots/user/profile.d/node.sh"
	assert_file "${HOME}/.config/dots/user/profile.d/deno.sh"
	assert_file "${HOME}/.config/dots/user/profile.d/rust.sh"
	# homebrew.sh is darwin-only; skip on linux test image.
	if [ "$(uname -s)" = Darwin ]; then
		assert_file "${HOME}/.config/dots/user/profile.d/homebrew.sh"
	fi
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

	# javascript package: bun + fnm + deno. fnm is the version manager;
	# node itself is installed via `fnm install` once a default version is
	# selected, so we don't assert `node` here -- only the runtimes the
	# package directly installs.
	assert_cmd bun
	assert_cmd fnm
	assert_cmd deno

	# editor package: cmd-edit (binary name `ed`). darwin uses the brew tap
	# formula; linux pulls the upstream tarball release. The bare assert_cmd
	# only confirms an `ed` binary is on PATH -- both cmd-edit and the GNU
	# line editor satisfy that. The follow-up `--version` probe distinguishes
	# the two: cmd-edit implements --version; GNU ed does not. If the probe
	# fails, the wrong `ed` is winning the PATH lookup.
	assert_cmd ed
	if ed --version >/dev/null 2>&1; then
		pass "ed --version succeeds (cmd-edit, not GNU ed)"
	else
		fail "ed --version failed (GNU ed shadowing cmd-edit on PATH?)"
	fi

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
