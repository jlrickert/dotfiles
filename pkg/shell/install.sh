#!/usr/bin/env bash
# Installs nvim configuration by symlink

PKG_ROOT="$(dirname "$(readlink -f "$0")")"
PROJECT_ROOT="$(cd "${PKG_ROOT}" && git rev-parse --show-toplevel)"

cd "${PKG_ROOT}" || return 1

. "${PROJECT_ROOT}/lib/env.sh"
. "${PROJECT_ROOT}/lib/func.sh"

ensure_environment

install_symlink "${PKG_ROOT}/bin" "${HOME}/bin"

blockinfile "${HOME}/.zshenv" zshenv ". ${PKG_ROOT}/lib/zshenv"
blockinfile "${HOME}/.zshrc" zshrc ". ${PKG_ROOT}/lib/zshrc"
