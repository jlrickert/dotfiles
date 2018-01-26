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

Running the install will create all the directories and symlink all files.
**WARNING**: this will remove pre-existing config files.

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
- fasd
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
- pkgfile
- powertop
- pulseaudio
- rlwrap
- rsync
- seahorse
- slop
- sshfs
- syncthing
- syncthing-inotify
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

- apache-tools
- discord
- gitflow-avh
- global
- google-chrome
- jdk
- package-query
- slack-desktop
- texlive-localmanager-git
- ttf-fira-code
- wemux-git
- yaourt

Programming related
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

### Elixer
- elixer

### Android

- android-studio
- android-tools
- android-udev
- android-file-transfer

Installation for other tools that I use.
----------------------------------------

### Setup Python

```bash
pacman -S python python-pip python-pipenv
pip install --user pipsi
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
pacman -S rustup
rustup install stable
cargo install rustfmt
cargo install racer
rustup component add rust-src # needed by racer
```

### Setup go

```bash
pacman -S go go-tools
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

### Setup Elixir

bunt - depedency for credo

```bash
git clone https://github.com/rrrene/bunt
cd bunt
mix deps.get
mix archive.build
mix archive.install
```

credo - static analysis

```bash
git clone https://github.com/rrrene/credo
cd credo
mix deps.get
mix archive.build
mix archive.install
```

System setup checklist
----------------------

- [ ] [base install example](https://gist.github.com/binaerbaum/535884a7f5b8a8697557)
- [ ] [Gnome keyring setup](https://wiki.archlinux.org/index.php/GNOME/Keyring#Using_the_keyring_outside_GNOME)
- [ ] at `sudo systemctl enable atd`
- [ ] networkmanager
- [ ] sshd
- [ ] ntp
- [ ] locale setup
- [ ] mkinitcpio
- [ ] daily mirrorlist update - `sudo systemctl enable reflector.timer`
