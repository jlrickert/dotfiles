#!/usr/bin/env bash

if ! [ -d $HOME/.oh-my-zsh ]; then
  git clone git@github.com:robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

if ! [ -d $HOME/.emacs.d ]; then
  git clone -b develop git@github.com:syl20bnr/spacemacs.git ~/.emacs.d
fi
