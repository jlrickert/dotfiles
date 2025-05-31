#!/usr/bin/env bash

dotsh install go

function _install_brew_package() {
	local package=$1

	if ! command -v brew &>/dev/null; then
		return 1
	fi

	if brew install "${package}"; then
		return 0
	else
		return 1
	fi
}

function _install_go_package() {
	local package=$1
	if ! command -v go &>/dev/null; then
		return 1
	fi

	if go install "${package}"; then
		return 0
	else
		return 1
	fi
}

function _install_mods() {
	if command -v mods &>/dev/null; then
		log_message INFO "mods is already installed."
		return 0
	fi

	if _install_brew_package charmbracelet/tap/mods; then
		return 0
	fi

	if _install_go_package github.com/charmbracelet/mods@latest; then
		return 0
	fi

	return 1
}

function _install_yq() {
	if command -v mods &>/dev/null; then
		log_message INFO "yq is already installed."
		return 0
	fi

	if _install_brew_package yq; then
		return 0
	fi

	if _install_go_package github.com/mikefarah/yq/v4@latest; then
		return 0
	fi

	return 1
}

_install_mods
_install_yq

mkdir -p "${HOME}/.config/mods"
install_symlink "${PACKAGE_ROOT}/mods.yml" "${HOME}/.config/mods/mods.yml"
