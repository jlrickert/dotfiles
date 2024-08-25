#!/usr/bin/bash env

set -o errexit  # exit if non-zero status code is returned
set -o nounset  # exit if undefined variable is used

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)

"${SCRIPT_DIR}/packages/chezmoi/install.sh" "$@"
