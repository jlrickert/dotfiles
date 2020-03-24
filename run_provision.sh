#!/usr/bin/env bash

if ! [ -d $HOME/.local/share/oh-my-zsh ]; then
  git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.local/share/oh-my-zsh
fi

if ! [ -d $HOME/.emacs.d ]; then
  git clone -b develop https://github.com/syl20bnr/spacemacs.git ~/.emacs.d
fi

if ! [ -d $HOME/.vim/bundle/Vundle.vim ]; then
  git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
  vim +PluginInstall +qall
fi

if [ -x "(command -v nvim)" ] && [ ! -d $HOME/.config/nvim/bundle/Vundle.vim ]; then
  git clone https://github.com/VundleVim/Vundle.vim.git ~/.config/nvim/bundle/Vundle.vim
  nvim +PluginInstall +qall
fi
