#!/usr/bin/env sh
# uninstall-git.sh — unset every key install-git.sh sets.
#
# Mirror of install-git.sh's owned-key list. Any key added there must be
# added here too. `|| true` after every --unset because git exits non-zero
# when a key is absent, and `set -e` would otherwise abort mid-stream.
#
# Does not remove the delta binary (a tool, not a config key) and does not
# touch any keys outside the owned list.

set -eu

# Identity.
git config --global --unset user.email 2>/dev/null || true
git config --global --unset user.name 2>/dev/null || true
git config --global --unset user.signingkey 2>/dev/null || true

# Core.
git config --global --unset core.editor 2>/dev/null || true
git config --global --unset core.autocrlf 2>/dev/null || true
git config --global --unset core.pager 2>/dev/null || true

# Delta.
git config --global --unset delta.side-by-side 2>/dev/null || true
git config --global --unset delta.line-numbers 2>/dev/null || true
git config --global --unset delta.navigate 2>/dev/null || true
git config --global --unset delta.syntax-theme 2>/dev/null || true
git config --global --unset interactive.diffFilter 2>/dev/null || true

# Color toggles.
git config --global --unset color.branch 2>/dev/null || true
git config --global --unset color.diff 2>/dev/null || true
git config --global --unset color.status 2>/dev/null || true

# Color "branch" subgroup.
git config --global --unset color.branch.current 2>/dev/null || true
git config --global --unset color.branch.local 2>/dev/null || true
git config --global --unset color.branch.remote 2>/dev/null || true

# Color "diff" subgroup.
git config --global --unset color.diff.meta 2>/dev/null || true
git config --global --unset color.diff.frag 2>/dev/null || true
git config --global --unset color.diff.old 2>/dev/null || true
git config --global --unset color.diff.new 2>/dev/null || true
git config --global --unset color.diff.plain 2>/dev/null || true

# Push / pull.
git config --global --unset push.default 2>/dev/null || true
git config --global --unset pull.default 2>/dev/null || true

# Init.
git config --global --unset init.defaultBranch 2>/dev/null || true

# Aliases.
git config --global --unset alias.st 2>/dev/null || true
git config --global --unset alias.stat 2>/dev/null || true
git config --global --unset alias.cm 2>/dev/null || true
git config --global --unset alias.cma 2>/dev/null || true
git config --global --unset alias.amend 2>/dev/null || true
git config --global --unset alias.caa 2>/dev/null || true
git config --global --unset alias.filelog 2>/dev/null || true
git config --global --unset alias.fl 2>/dev/null || true
git config --global --unset alias.ch 2>/dev/null || true

# git-lfs filter.
git config --global --unset filter.lfs.required 2>/dev/null || true
git config --global --unset filter.lfs.clean 2>/dev/null || true
git config --global --unset filter.lfs.smudge 2>/dev/null || true
git config --global --unset filter.lfs.process 2>/dev/null || true

# GPG / signing.
git config --global --unset gpg.format 2>/dev/null || true
git config --global --unset commit.gpgsign 2>/dev/null || true

# Modern defaults.
git config --global --unset pull.rebase 2>/dev/null || true
git config --global --unset push.autoSetupRemote 2>/dev/null || true
git config --global --unset diff.colorMoved 2>/dev/null || true
git config --global --unset merge.conflictstyle 2>/dev/null || true
