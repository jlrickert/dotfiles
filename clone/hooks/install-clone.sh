#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

# Drop the legacy `clone` shell helper installed by the pre-dots layout
# (monolithic shell script under pkg/00_shell). Idempotent: -f swallows
# the missing-file case on first install or re-runs.
echo "clone: removing legacy pre-dots clone helper if present"
rm -f "${HOME}/.local/state/dotfiles/pkg/00_shell/bin/clone"

# Belt-and-braces: ensure the new clone entry-points are executable. dots
# uses link_strategy=copy here, and exec-bit preservation across that copy
# is implementation-dependent; chmod here so the bins always work
# post-install. _clone_lib.py is imported, never exec'd, so it is
# deliberately left alone.
if [ -e "${HOME}/.local/bin/gh-clone" ]; then
	echo "clone: ensuring gh-clone is executable"
	chmod +x "${HOME}/.local/bin/gh-clone"
fi
if [ -e "${HOME}/.local/bin/bb-clone" ]; then
	echo "clone: ensuring bb-clone is executable"
	chmod +x "${HOME}/.local/bin/bb-clone"
fi
