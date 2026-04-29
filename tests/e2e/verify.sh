#!/usr/bin/env bash
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=lib.sh
. "${HERE}/lib.sh"

cd "${HOME}"

# link_strategy=copy (set in dots-config/config.yaml), so installed
# dotfiles land as regular files.
assert_file "${HOME}/.zshrc"
assert_file "${HOME}/.zshenv"
assert_file "${HOME}/.bashrc"
assert_file "${HOME}/.bash_profile"
assert_file "${HOME}/.profile"
assert_file "${HOME}/.config/dots/config.yaml"
# zsh stubs redirect to ~/.config/zsh/, where the real configs live.
assert_file "${HOME}/.config/zsh/zshenv"
assert_file "${HOME}/.config/zsh/zshrc"

assert_cmd starship
assert_cmd fzf
assert_cmd rg
# Debian/Ubuntu rename these binaries; the install hook installs them under
# their distro-renamed names.
assert_cmd batcat
assert_cmd fdfind

assert_shell_loads bash
assert_shell_loads zsh

summary
