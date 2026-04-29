#!/usr/bin/env bash
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=lib.sh
. "${HERE}/lib.sh"

cd "${HOME}"

# Verify scripts and ~/.local/bin-installed tools (zellij on linux,
# set-zellij-colorscheme via @bin) need this on PATH; the test entrypoint
# does not source a login shell.
export PATH="${HOME}/.local/bin:${PATH}"

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
# Debian/Ubuntu rename these binaries; the install hook installs them under
# their distro-renamed names.
assert_cmd batcat
assert_cmd fdfind

assert_shell_loads bash
assert_shell_loads zsh

summary
