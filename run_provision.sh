#!/usr/bin/env bash

if ! [ -d $HOME/.oh-my-zsh ]; then
  git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

if ! [ -d $HOME/.emacs.d ]; then
  git clone -b develop https://github.com/syl20bnr/spacemacs.git ~/.emacs.d
fi

if ! [ -d $HOME/.vim ]; then
  git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
  vim +PluginInstall +qall
fi
