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
# Phase-3 migrations: claude/, codex/, editor/, rust/,
# javascript/. The Ubuntu test images don't install these packages
# yet (see containers/ubuntu/Dockerfile), so we only assert the manifests
# exist in the source tree — proof the migration landed without trying
# to verify runtime side effects. Add runtime assertions here once the
# image installs them.
DOTFILES_SRC="${DOTFILES_SRC:-/opt/dotfiles-src}"
for pkg in claude codex editor rust javascript wezterm python clone podman; do
	assert_file "${DOTFILES_SRC}/${pkg}/Dotfile.yaml"
done

# intellij package — runtime assertions, gated on the package having been
# installed. The package's `links:` deposits ~/.ideavimrc, so its presence is
# a reliable "did the package install?" probe. The Ubuntu test images don't
# install intellij/ yet, so this skips silently in CI.
if [ -f "${HOME}/.ideavimrc" ]; then
	assert_file "${HOME}/.ideavimrc"
	[ -s "${HOME}/.ideavimrc" ] || fail "ideavimrc empty"

	assert_file "${HOME}/.local/bin/ide"
	[ -x "${HOME}/.local/bin/ide" ] || fail "ide not executable"
	assert_cmd ide

	ide --help >/dev/null 2>&1 || fail "ide --help failed"
	ide help >/dev/null 2>&1 || fail "ide help failed"

	# Regression guard: the launcher must spawn the IDE detached, not exec it.
	# `exec "${bin}"` would replace the parent shell with the IntelliJ process,
	# attaching it to the tty (ctrl-c kills the IDE, closing the terminal kills
	# it, stdout/stderr are captured). The fix replaces both exec sites with a
	# launch_detached helper.
	if grep -qE 'exec "\$\{bin\}"' "${HOME}/.local/bin/ide"; then
		fail "ide still exec's launcher (regression: parent shell will be replaced)"
	else
		pass "ide does not exec launcher (spawn-and-return preserved)"
	fi

	assert_file "${HOME}/.config/bash/completions/ide.bash"
	assert_file "${HOME}/.config/zsh/completions/_ide"
fi

assert_file "${HOME}/.config/starship.toml"
if [ "${VERIFY_PROFILE}" = full ]; then
	assert_file "${HOME}/.config/zellij/config.kdl"
	assert_cmd zellij
	assert_cmd mux
	assert_cmd set-zellij-colorscheme
	# Theme contract: config.kdl vendors both `kanagawa-wave` and
	# `kanagawa-lotus`. The active `theme "..."` line is rewritten at runtime
	# by set-zellij-colorscheme; prefix match accepts either variant.
	assert_grep "${HOME}/.config/zellij/config.kdl" 'theme "kanagawa'
	# Launch smoke: `assert_cmd zellij` only checks PATH, which would pass
	# even when zellij can't start (e.g. config references an undefined
	# theme). `setup --check` parses config and validates themes — the cheap
	# guard against breaking interactive launches without noticing.
	if zellij setup --check >/dev/null 2>&1; then
		pass "zellij setup --check passes"
	else
		fail "zellij setup --check failed (config or theme issue)"
	fi
fi
# Real zsh configs live under ~/.config/zsh/.
assert_file "${HOME}/.config/zsh/zshenv"
assert_file "${HOME}/.config/zsh/zshrc"
assert_file "${HOME}/.config/zsh/zprofile"
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
# common-shell ships a `la` script under ~/.local/bin so `la` works
# identically across bash/zsh/POSIX shells without alias expansion. The
# script must be executable (link_strategy=copy preserves the mode bit),
# behave like `ls -lAh` (header line "total NN"), and the legacy `alias la=`
# lines must be gone from the rc files so the script wins on PATH.
assert_file "${HOME}/.local/bin/la"
if [ -x "${HOME}/.local/bin/la" ]; then
	pass "${HOME}/.local/bin/la is executable"
else
	fail "${HOME}/.local/bin/la is not executable"
fi
if "${HOME}/.local/bin/la" "${HOME}" 2>/dev/null | head -n1 | grep -q '^total '; then
	pass "la behavioral smoke (\"total NN\" header)"
else
	fail "la behavioral smoke failed (no \"total NN\" header)"
fi
assert_file "${HOME}/.config/bash/completions/la.bash"
assert_file "${HOME}/.config/zsh/completions/_la"
# Regression guard: the script on PATH must not be shadowed by an alias.
if grep -qE "^alias la=" "${HOME}/.bashrc"; then
	fail "${HOME}/.bashrc still defines 'alias la='"
else
	pass "${HOME}/.bashrc has no 'alias la=' line"
fi
if grep -qE "^alias la=" "${HOME}/.config/zsh/zshrc"; then
	fail "${HOME}/.config/zsh/zshrc still defines 'alias la='"
else
	pass "${HOME}/.config/zsh/zshrc has no 'alias la=' line"
fi
# ~/.zshrc and ~/.zshenv are NOT package files -- the install hook injects
# a marker block that sources ~/.config/zsh/. Verify the block is present.
assert_grep "${HOME}/.zshrc" "BEGIN dotfiles-zsh"
assert_grep "${HOME}/.zshenv" "BEGIN dotfiles-zsh"
# Layer-1 marker pattern: ~/.profile must contain the dotfiles-profile block.
assert_grep "${HOME}/.profile" "BEGIN dotfiles-profile"
# Real Layer-1 file lives under ~/.config/dots/user/.
assert_file "${HOME}/.config/dots/user/profile"
# Startup-perf guard: brew shellenv must be inlined, not eval'd. The eval
# form spawns brew (Ruby, ~50-80ms) on every shell init and re-invokes
# path_helper, which would re-trigger the login-shell PATH demotion fixed
# in commit af6c1ec.
if grep -qE 'eval "\$\(/opt/homebrew/bin/brew shellenv\)"' "${HOME}/.config/dots/user/profile"; then
	fail "brew shellenv eval form found (should be inlined)"
else
	pass "brew shellenv inlined (no eval spawn)"
fi
# Darwin-only positive assertion: the inlined Apple-Silicon prefix block
# must be present in the profile source. Skipped on Linux because the
# block is darwin-only env material.
if [ "$(uname -s)" = Darwin ]; then
	assert_grep "${HOME}/.config/dots/user/profile" 'HOMEBREW_PREFIX="/opt/homebrew"'
fi
# bat theme: common-shell ships hand-derived kanagawa tmThemes and defaults
# BAT_THEME to kanagawa_wave. The theme file must land at the bat user-config
# path (link_strategy=copy → real file, not symlink), and the installed profile
# must export the default.
assert_file "${HOME}/.config/bat/themes/kanagawa_wave.tmTheme"
assert_grep "${HOME}/.config/dots/user/profile" 'BAT_THEME:=kanagawa_wave'
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
# Startup-perf guard: the cached compinit fast path must be present, so
# warm interactive shells skip the ~250ms zcompdump rebuild + security
# check (the (#qNmh-24) glob qualifier rebuilds at most once per 24h).
assert_grep "${HOME}/.config/zsh/zshrc" "compinit -C -d"
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
	# podman package: darwin-only. The fragment must land regardless of
	# whether Podman Desktop's .dmg has been installed (the fragment
	# self-guards on `[ -d /opt/podman/bin ]`). The runtime probe
	# (`podman --version`) only fires when the bundle is actually present,
	# so fresh runners without the .dmg stay silent.
	if [ "$(uname -s)" = Darwin ]; then
		assert_file "${HOME}/.config/dots/user/profile.d/podman.sh"
		assert_grep "${HOME}/.config/dots/user/profile.d/podman.sh" "/opt/podman/bin"
		if [ -d /opt/podman/bin ]; then
			assert_cmd podman
			if podman --version >/dev/null 2>&1; then
				pass "podman --version succeeds"
			else
				fail "podman --version failed"
			fi
		fi
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
	# Darwin-only regression guard: /etc/zprofile runs `path_helper`, which
	# demotes /opt/homebrew/bin (and other Layer-1 prepends) below /bin and
	# /usr/bin. Login zsh shells (wezterm, Terminal.app) hit this; the non-
	# login `ed --version` probe above does not. The fix re-sources ~/.profile
	# from ~/.zprofile -- this assertion proves the fix lands by checking that
	# `command -v ed` from a login zsh resolves under a Homebrew prefix.
	# Skipped on Linux because there is no path_helper there.
	if [ "$(uname -s)" = Darwin ]; then
		login_ed="$(zsh -l -c 'command -v ed' 2>/dev/null || true)"
		case "${login_ed}" in
		/opt/homebrew/bin/ed | /usr/local/bin/ed)
			pass "login zsh resolves ed to Homebrew (${login_ed})"
			;;
		*)
			fail "login zsh resolves ed to '${login_ed:-<unset>}' (expected Homebrew prefix; path_helper demotion not corrected)"
			;;
		esac
		# Generalized front-of-PATH guard: the af6c1ec fix only covered brew
		# because `ed` collides with /bin/ed. The same path_helper demotion
		# silently affected every Layer-1 prepend (~/.local/bin, ~/.bun/bin,
		# etc.) — they sat ~25 entries deep behind /bin in login zsh. The
		# 0.5.5 fix narrowed all guards from "anywhere in PATH" to "at the
		# front of PATH" so the re-source from ~/.zprofile re-promotes them.
		# Assertion: in a login zsh, each guarded dir must appear EARLIER in
		# PATH than /bin. Skipped on Linux because there is no path_helper.
		login_path="$(zsh -l -c 'echo $PATH' 2>/dev/null || echo)"
		local_idx=-1
		bun_idx=-1
		podman_idx=-1
		bin_idx=-1
		i=0
		IFS=:
		for entry in ${login_path}; do
			case "${entry}" in
			"${HOME}/.local/bin") [ ${local_idx} -lt 0 ] && local_idx=${i} ;;
			"${HOME}/.bun/bin") [ ${bun_idx} -lt 0 ] && bun_idx=${i} ;;
			/opt/podman/bin) [ ${podman_idx} -lt 0 ] && podman_idx=${i} ;;
			/bin) [ ${bin_idx} -lt 0 ] && bin_idx=${i} ;;
			esac
			i=$((i + 1))
		done
		unset IFS
		if [ ${local_idx} -ge 0 ] && [ ${bin_idx} -ge 0 ] && [ ${local_idx} -lt ${bin_idx} ]; then
			pass "login zsh PATH: ~/.local/bin (idx ${local_idx}) precedes /bin (idx ${bin_idx})"
		else
			fail "login zsh PATH: ~/.local/bin (idx ${local_idx}) not before /bin (idx ${bin_idx}); path_helper demotion not corrected"
		fi
		# Bun is optional — only assert if ~/.bun exists on the host.
		if [ -d "${HOME}/.bun" ]; then
			if [ ${bun_idx} -ge 0 ] && [ ${bin_idx} -ge 0 ] && [ ${bun_idx} -lt ${bin_idx} ]; then
				pass "login zsh PATH: ~/.bun/bin (idx ${bun_idx}) precedes /bin (idx ${bin_idx})"
			else
				fail "login zsh PATH: ~/.bun/bin (idx ${bun_idx}) not before /bin (idx ${bin_idx}); path_helper demotion not corrected"
			fi
		fi
		# Podman Desktop's bundle is optional — only assert if /opt/podman/bin
		# exists on the host. profile.d/podman.sh self-guards on the same
		# directory check, so fresh runners without the .dmg installed
		# legitimately have no /opt/podman/bin entry on PATH.
		if [ -d /opt/podman/bin ]; then
			if [ ${podman_idx} -ge 0 ] && [ ${bin_idx} -ge 0 ] && [ ${podman_idx} -lt ${bin_idx} ]; then
				pass "login zsh PATH: /opt/podman/bin (idx ${podman_idx}) precedes /bin (idx ${bin_idx})"
			else
				fail "login zsh PATH: /opt/podman/bin (idx ${podman_idx}) not before /bin (idx ${bin_idx}); path_helper demotion not corrected"
			fi
		fi
	fi

	# editor package: nvim. We install upstream tarball v0.12.x on Linux
	# (jlrickert/kickstart.nvim targets 0.12+ APIs); the PPA build is too
	# old. Floor at major=0, minor>=12. Same shape as the python3 >= 3.11
	# probe above: capture (major, minor), bail with informative fail if the
	# version line doesn't parse, otherwise compare with shell arithmetic.
	assert_cmd nvim
	nvim_version_line="$(nvim --version 2>/dev/null | head -n1)"
	# "NVIM v0.12.2" → "0.12.2" → major=0 minor=12.
	nvim_dotted="$(printf '%s' "${nvim_version_line}" | awk '{print $2}' | sed 's/^v//')"
	nvim_major="$(printf '%s' "${nvim_dotted}" | cut -d. -f1)"
	nvim_minor="$(printf '%s' "${nvim_dotted}" | cut -d. -f2)"
	if [ -n "${nvim_major}" ] && [ -n "${nvim_minor}" ] &&
		[ "${nvim_major}" -eq 0 ] 2>/dev/null && [ "${nvim_minor}" -ge 12 ] 2>/dev/null; then
		pass "nvim >= 0.12 (${nvim_dotted})"
	elif [ -n "${nvim_major}" ] && [ "${nvim_major}" -ge 1 ] 2>/dev/null; then
		# Future-proofing: any 1.x release is also >= 0.12.
		pass "nvim >= 0.12 (${nvim_dotted})"
	else
		fail "nvim below 0.12 floor (${nvim_version_line:-unparseable})"
	fi
	# Headless smoke: load lua, print, quit. Catches a broken runtime tree
	# (e.g. tarball cp that missed share/nvim/runtime/) that --version alone
	# wouldn't notice.
	if nvim --headless "+lua print('ok')" +qall >/dev/null 2>&1; then
		pass "nvim --headless lua smoke"
	else
		fail "nvim --headless lua smoke failed"
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
