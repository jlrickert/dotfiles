#!/bin/bash

set -o errexit  # exit if non-zero status code is returned
set -o nounset  # exit if undefined variable is used
set -o pipefail # exit if no-zero status code is returned in a pipeline
IFS=$'\n\t'

if ! test -f vault.key; then
    echo -n "Enter your password: "
    read -s password
    echo
    echo $password >vault.key
fi
