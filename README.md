# dotfiles
My personal dotfiles

User preference and configuration files that I use to build my working
environment.

This repository includes [git](http://git-scm.com/) submodules. When cloning to
a new machine, the submodules need to be initialized.

		$ git submodule update --init

Every submodule can be updated to the latest available version at once:

		$ git submodule foreach git pull origin master

I Recemend using rcm for installation:

    $ rcup -x README.md

# Post install

install rvm with:

    $ gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
    $ curl -sSL https://get.rvm.io | bash -s stable
