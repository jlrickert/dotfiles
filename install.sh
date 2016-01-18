#!/bin/bash
DOTFILES=$PWD

CONFIG=$DOTFILES/config/*
for f in $CONFIG
do
  file=$(basename $f)
  ln -sf $f ~/.$file
done

ZSH_FILES=$DOTFILES/zsh
ln -snf $ZSH_FILES ~/.config/zsh

i3_FILES=$DOTFILES/i3
ln -snf $i3_FILES ~/.config/i3
echo "ln -snf ${i3_FILES}/i3{labtop,desktop}-bar ~/.config/i3status"

BIN_PATH=$DOTFILES/bin
ln -snf $BIN_PATH ~/bin

mkdir -p ~/.config/termite
ln -snf $DOTFILES/xdgconfig/termite/config ~/.config/termite/config

EMACS_PATH=$DOTFILES/emacs
ln -snf $EMACS_PATH ~/.emacs.d
