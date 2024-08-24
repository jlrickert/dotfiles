#!/usr/bin/bash env

set -o errexit  # exit if non-zero status code is returned
set -o nounset  # exit if undefined variable is used

sh -c "$(curl -fsLS get.chezmoi.io/lb)" -- init --apply jlrickert "$@"
