#!/usr/bin/env bash
# Installs nvim configuration by symlink

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}" && git rev-parse --show-toplevel)"

cd "${SCRIPT_DIR}" || return 1

. "${PROJECT_ROOT}/lib/env.sh"
. "${PROJECT_ROOT}/lib/func.sh"

ensure_environment

install_symlink "${SCRIPT_DIR}/bin" "${HOME}/bin"

ensure_block "${HOME}/.zshenv" < "${SCRIPT_DIR}/lib/zshenv"
ensure_block "${HOME}/.zshrc" < "${SCRIPT_DIR}/lib/zshrc"
