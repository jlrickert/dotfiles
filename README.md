My personal dotfiles
====================

User preference and configuration files that I use to build my working
environment.

Installation
------------

### Init submodules

This repository includes [git](http://git-scm.com/) submodules. When cloning to
a new machine, the submodules need to be initialized.

```bash
git submodule update --init
```

### Update submodules
Every submodule can be updated to the latest available version at once:

```bash
git submodule foreach git pull origin master
```

### Symlink everthing
I Recommend using [rcm](https://github.com/thoughtbot/rcm) for installation:

```bash
rcup -x README.md`
```

### Install zprezto:

```bash
git clone --recursive https://github.com/sorin-ionescu/prezto.git $HOME/.zprezto
chsh -s /bin/zsh
```

---

Installation for other tools that I use.
----------------------------------------

## rvm

install rvm with:

```bash
gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
curl -sSL https://get.rvm.io | bash -s stable
```

install pyenv with:

```bash
git clone https://github.com/yyuu/pyenv.git ~/.pyenv
```
