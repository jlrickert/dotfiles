#!/bin/bash

set -o errexit  # exit if non-zero status code is returned
set -o nounset  # exit if undefined variable is used
set -o pipefail # exit if no-zero status code is returned in a pipeline
IFS=$'\n\t'

export PATH="$PATH:~/.local/bin:$PWD/dots/scripts"

if ! test -f vault.key; then
    echo -n "Enter your password: "
    read -s password
    echo
    echo $password >vault.key
    chmod 0600 vault.key
fi