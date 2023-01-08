#!/bin/sh
set -e

_symlink() {
    mkdir -p "$(dirname "$2")"
    ln -sf "$1" "$2"
}

HERE="$(cd "$(dirname "$0")" && pwd)"
cd "$HERE"

_symlink "$HERE/.bashrc" "$HOME/.bashrc"
_symlink "$HERE/.profile" "$HOME/.profile"
_symlink "$HERE/.dircolors" "$HOME/.dircolors"
_symlink "$HERE/.inputrc" "$HOME/.inputrc"
_symlink "$HERE/.zshrc" "$HOME/.zshrc"
_symlink "$HERE/.zshenv" "$HOME/.zshenv"
_symlink "$HERE/scripts" "$HOME/Scripts"

for i in git nvim tmux vim; do
    cd $i && ./setup
    cd -
done
