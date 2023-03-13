#!/bin/bash
set -e

_cd() {
	cd "$1" &>/dev/null
}

_symlink() {
	d=$(dirname "$2")
	test -d "$d" || mkdir -p "$d"
	ln -sf "$1" "$2"
}

_symlink_dir() {
	test -d "$2" && rm -r "$2"
	d=$(dirname "$2")
	test -d "$d" || mkdir -p "$d"
	ln -sf "$1" "$2"
}

HERE="$(cd "$(dirname "$0")" && pwd)"
cd "$HERE"

source "$HERE/.zshenv"

_symlink "$HERE/.bashrc" "$HOME/.bashrc"
_symlink "$HERE/.profile" "$HOME/.profile"
_symlink "$HERE/.dircolors" "$HOME/.dircolors"
_symlink "$HERE/.inputrc" "$HOME/.inputrc"
_symlink "$HERE/.zshrc" "$HOME/.zshrc"
_symlink "$HERE/.zshenv" "$HOME/.zshenv"
_symlink "$HERE/.tmux.conf" "$HOME/.tmux.conf"
_symlink_dir "$HERE/scripts" "$SCRIPTS"

for i in git nvim vim kitty; do
	_cd $i && ./setup
	_cd -
done
