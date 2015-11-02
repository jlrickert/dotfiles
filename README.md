# dotfiles
My personal dotfiles

# 1. PREREQUISITES

requires:
  - zsh
  
## 2. INSTALLATION

Run these 3 commands:

Clone dotfiles:

    git clone https://github.com/jlrickert/dotfiles ~/.dotfiles
    
Install prezto:

    git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"

Symlinks files - you may want to check it out and back up stuff:

    cd ~/.dotfiles && sh install.sh
    

## 3. Upgrading

To upgrade prezto:

    git pull && git submodule update --init --recursive
