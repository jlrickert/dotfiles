#!/usr/bin/env bash

if [[ ! -r "${HOME}/.local/state/dotfiles/docker-bootstrapped" ]]; then
	BIN="${HOME}/dotfiles/bin/dotsh"

	"${BIN}" bootstrap
	"${BIN}" install 00_shell
	"${BIN}" install 10_zsh
	"${BIN}" install 10_fish
	"${BIN}" install 20_go
	"${BIN}" install 20_deno
	"${BIN}" install 20_rust
	"${BIN}" install 80_editor
	"${BIN}" install 80_knut

	touch "${HOME}/.local/state/dotfiles/docker-bootstrapped"
fi

zsh "$@"
