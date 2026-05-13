#!/usr/bin/env sh
# install-git.sh — package-owned git config keys.
#
# The keys set below are the SOLE source of truth for what this package owns.
# uninstall-git.sh mirrors this exact list. Add/remove keys here first, then
# mirror the change in uninstall-git.sh.
#
# Per-machine overrides:
#   - user.signingkey defaults to ~/.ssh/id_ed25519.pub. Machines without an
#     ed25519 key at that path must override per-machine after install:
#         git config --global user.signingkey <path>
#   - delta is installed below (brew on darwin, upstream tarball on linux).
#     If delta is unavailable, override core.pager and unset delta.* keys.

set -eu

# ---- package-owned keys ----

# Identity.
git config --global user.email "jaredrickert52@gmail.com"
git config --global user.name "Jared Rickert"
# git itself resolves the literal ~ at use time; we deliberately store the
# unexpanded form so the value stays portable across machines.
# shellcheck disable=SC2088
git config --global user.signingkey "~/.ssh/id_ed25519.pub"

# Core.
git config --global core.editor "nvim"
git config --global core.autocrlf "input"
git config --global core.pager "delta"

# Delta (diff pager).
git config --global delta.side-by-side "true"
git config --global delta.line-numbers "true"
git config --global delta.navigate "true"
git config --global delta.syntax-theme "kanagawa_wave"
git config --global interactive.diffFilter "delta --color-only"

# Color toggles.
git config --global color.branch "auto"
git config --global color.diff "auto"
git config --global color.status "auto"

# Color "branch" subgroup.
git config --global color.branch.current "red reverse"
git config --global color.branch.local "blue"
git config --global color.branch.remote "green"

# Color "diff" subgroup.
git config --global color.diff.meta "yellow"
git config --global color.diff.frag "magenta"
git config --global color.diff.old "red bold"
git config --global color.diff.new "green"
git config --global color.diff.plain "white"

# Push / pull.
git config --global push.default "current"
git config --global pull.default "current"

# Init.
git config --global init.defaultBranch "main"

# Aliases.
git config --global alias.st "status"
git config --global alias.stat "status"
git config --global alias.cm "commit -m"
git config --global alias.cma "commit -a -m"
git config --global alias.amend "commit --amend"
git config --global alias.caa "commit -a --amend -C HEAD"
git config --global alias.filelog "log -u"
git config --global alias.fl "log -u"
git config --global alias.ch "checkout"

# git-lfs filter.
git config --global filter.lfs.required "true"
git config --global filter.lfs.clean "git-lfs clean -- %f"
git config --global filter.lfs.smudge "git-lfs smudge -- %f"
git config --global filter.lfs.process "git-lfs filter-process"

# GPG / signing.
git config --global gpg.format "ssh"
git config --global commit.gpgsign "true"

# ---- Modern defaults (added by the dotfiles package; not in the original
#      legacy gitconfig). Kept separate so it's obvious these were chosen
#      here rather than imported.
git config --global pull.rebase "false"
git config --global push.autoSetupRemote "true"
git config --global diff.colorMoved "default"
git config --global merge.conflictstyle "zdiff3"

# Unset legacy core.excludesfile — git auto-detects ~/.config/git/ignore per
# the XDG spec, and this package places its global ignore file there. The
# legacy excludesfile = ~/.gitignore_global key would otherwise shadow it.
# `|| true` because `--unset` exits non-zero when the key is absent and
# `set -e` would abort the rest of the install.
git config --global --unset core.excludesfile 2>/dev/null || true

# ---- delta install ----
if ! command -v delta >/dev/null 2>&1; then
	case "$(uname -s)" in
	Darwin)
		if command -v brew >/dev/null 2>&1; then
			brew install git-delta
		else
			echo "git: Homebrew not found; install git-delta manually." >&2
		fi
		;;
	Linux)
		arch="$(uname -m)"
		case "${arch}" in
		x86_64) asset_arch="x86_64-unknown-linux-gnu" ;;
		aarch64 | arm64) asset_arch="aarch64-unknown-linux-gnu" ;;
		*)
			echo "git: unsupported arch ${arch} for delta install" >&2
			exit 1
			;;
		esac
		version="0.18.2"
		tmpdir="$(mktemp -d)"
		trap 'rm -rf "$tmpdir"' EXIT
		curl -fsSL "https://github.com/dandavison/delta/releases/download/${version}/delta-${version}-${asset_arch}.tar.gz" \
			| tar -xz -C "${tmpdir}"
		mkdir -p "${HOME}/.local/bin"
		install -m 0755 "${tmpdir}/delta-${version}-${asset_arch}/delta" "${HOME}/.local/bin/delta"
		;;
	*)
		echo "git: unsupported OS '$(uname -s)' for delta install; skipping." >&2
		;;
	esac
fi
