#!/usr/bin/env bash

case "$(uname -s)" in
Darwin)
	if ! have brew fish; then
		brew install fish
	fi
	;;
*) ;;
esac

mkdir -p "${XDG_CONFIG_HOME}/fish"

install_symlink "${PACKAGE_ROOT}/config.fish" "${XDG_CONFIG_HOME}/fish/config.fish"
