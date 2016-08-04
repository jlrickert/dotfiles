My personal dotfiles
====================

My preferences and configuration for my current working environment.

Installation
------------

### Downloading

```bash
git clone https://github.com/jlrickert/dotfiles.git ~/dotfiles
```


### Installation

Running the install will create all the directories and symlink all files
```bash
python install.py
```

#### Installing prezto

Run the following and don't worry about what zprezto says in their readme.

```bash
git clone --recursive https://github.com/sorin-ionescu/prezto.git $HOME/.zprezto
chsh -s /bin/zsh
```

---

Pacman packages that I use.
---------------------------

## Core

- abs
- at
- dmenu
- dunst
- emacs
- feh
- filezilla
- firefox
- firejail
- git
- gnome-keyring
- htop
- i3-wm
- i3lock
- i3status
- mlocate
- neovim
- networkmanager
- networkmanager-applet
- networkmanager-openvpn
- nm-connection-editor
- nmap
- numlockx
- pandoc
- powertop
- pulseaudio
- rlwrap
- rsync
- seahorse
- tcpdump
- teamspeak
- termite
- the_silver-searcher
- tree
- unclutter
- xorg-xset
- zsh
- zsh-completions
- zsh-syntax-highlighting

## AUR

- fasd
- global
- google-chrome
- jdk
- package-query
- rcm
- yaourt

## Programing related

### C/C++

- clang
- gdb

### Python

- autopep8
- flake8
- python-jedi
- python-rope
- yapf

### Lisp

- buildapp
- clisp
- sbcl

### clojure

- leiningen

### Scala

- scala
- sbt

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
git clone https://github.com/yyuu/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
```

System setup checklist
----------------------

## Gnome-keyring

- [ ] [ Gnome keyring setup](https://wiki.archlinux.org/index.php/GNOME/Keyring#Using_the_keyring_outside_GNOME)
- [ ] at `systemctl enable atd`
- [ ] networkmanager
- [ ] sshd
- [ ] ntp
- [ ] locale setup
- [ ] mkinitcpio
