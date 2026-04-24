#!/usr/bin/env bash
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=lib.sh
. "${HERE}/lib.sh"

cd "${HOME}"

assert_link "${HOME}/.zshrc"
assert_link "${HOME}/.zshenv"
assert_link "${HOME}/.bashrc"
assert_link "${HOME}/.bash_profile"
assert_link "${HOME}/.profile"
# dots 0.4.0 copies (rather than symlinks) the bootstrap dots-config into place.
assert_file "${HOME}/.config/dots/config.yaml"

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
