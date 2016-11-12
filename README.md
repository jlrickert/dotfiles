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

---

Pacman packages that I use.
---------------------------

### Core

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
- gnuplot
- htop
- i3-wm
- i3lock
- i3status
- inotify-tools
- maim
- mlocate
- namcap
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
- slop
- tcpdump
- teamspeak
- termite
- the_silver_searcher
- tree
- unclutter
- wireshark
- xdotools
- xorg-xset
- zsh
- zsh-completions
- zsh-syntax-highlighting

### AUR

- chromedriver
- fasd
- global
- google-chrome
- jdk
- package-query
- yaourt

Programing related
------------------

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
- roswell
- sbcl

### clojure

- leiningen

### Scala

- scala
- sbt

### Haskell

- stack

### Perl

- cpanminus

### Android

- android-studio
- android-tools
- android-udev
- android-file-transfer

Installation for other tools that I use.
----------------------------------------

### Setup Python

```bash
git clone https://github.com/yyuu/pyenv.git ~/.pyenv
git clone https://github.com/yyuu/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
```

### Setup Haskell

```bash
stack update
stack install alex hasktags happy stylish-haskell present ghc-mod hlint hoogle hindent
```

### Setup elm

```bash
sudo npm install -g elm elm-oracle
```

### Setup Ruby

```bash
git clone https://github.com/rbenv/rbenv.git ~/.rbenv
cd ~/.rbenv && src/configure && make -C src
git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build
```

### Setup rust

```bash
curl https://sh.rustup.rs -sSf | sh
cargo install rust-fmt
cargo install racer
```

### Setup go

```bash
go get -u -v github.com/nsf/gocode
go get -u -v github.com/rogpeppe/godef
go get -u -v golang.org/x/tools/cmd/oracle
go get -u -v golang.org/x/tools/cmd/gorename
go get -u -v golang.org/x/tools/cmd/goimports
```

### Setup Android

```bash
sudo groupadd sdkusers
sudo gpasswd -a $USER sdkusers
newgrp sdkusers

sudo gpasswd -a $USER adbusers

# after installing android sdk
sudo chown -R :sdkusers /opt/android-sdk/
sudo chmod -R g+w /opt/android-sdk/
```

System setup checklist
----------------------

### Gnome-keyring

- [ ] [Gnome keyring setup](https://wiki.archlinux.org/index.php/GNOME/Keyring#Using_the_keyring_outside_GNOME)
- [ ] at `systemctl enable atd`
- [ ] networkmanager
- [ ] sshd
- [ ] ntp
- [ ] locale setup
- [ ] mkinitcpio
