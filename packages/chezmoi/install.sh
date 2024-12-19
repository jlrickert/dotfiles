#!/usr/bin/env bash

set -o errexit  # exit if non-zero status code is returned
set -o nounset  # exit if undefined variable is used
set -o pipefail # exit if no-zero status code is returned in a pipeline

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
PROJECT_DIR=$(cd "${SCRIPT_DIR}/../.." &>/dev/null && pwd)
CHEZMOI_BIN="${HOME}/.local/bin/chezmoi"

# Installs chezmoi to ~/.local/bin/chezmoi if it doesn't exist
if [[ ! -x "${CHEZMOI_BIN}" ]]; then
	sh -c "$(curl -fsLS get.chezmoi.io/lb)"
fi

"${CHEZMOI_BIN}" init --apply --source "${PROJECT_DIR}" --destination "${HOME}/.local/share/dotfiles" "$@"
