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

BIN_PATH=$DOTFILES/bin
ln -snf $BIN_PATH ~/bin

mkdir -p ~/.config/termite
ln -snf $DOTFILES/xdgconfig/termite/config ~/.config/termite/config

