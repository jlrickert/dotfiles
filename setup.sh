#!/bin/bash

set -o errexit # exit if non-zero status code is returned
set -o nounset # exit if undefined variable is used

CHEZMOI_BIN=$(command -v chezmoi || echo ~/.local/bin/chezmoi)

# Installs chezmoi to ~/.local/bin/chezmoi if it doesn't exist
if [ ! -x "${CHEZMOI_BIN}" ]; then
	sh -c "$(curl -fsLS https://get.chezmoi.io/lb)"
fi

DOTFILES_ROOT="${HOME}/.local/share/dotfiles" 
if [ ! -d "${DOTFILES_ROOT}" ]; then
	git clone https://github.com/jlrickert/dotfiles "${DOTFILES_ROOT}"
fi

"${DOTFILES_ROOT}/packages/chezmoi/install.sh"
