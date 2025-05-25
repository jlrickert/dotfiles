# Dotfiles of Jared Rickert

My personal dotfiles. Pilfer at your own peril 😈

## Installation

### Docker

Docker installation required. See pkg/docker for more details.

```bash
. env
dotsh boostrap
```

### Ubuntu

```bash
sudo apt install -y git curl zsh
sh -c "$(curl -fsLS https://raw.githubusercontent.com/jlrickert/dotfiles/main/setup.sh)"
```

### MacOSX

```bash
brew install curl git # I am guessing here. Too lazy to look it up
sh -c "$(curl -fsLS https://raw.githubusercontent.com/jlrickert/dotfiles/main/setup.sh)"
```

### Manual

```bash
git clone https://github.com/jlrickert/dotfiles "${HOME}/.config/dotfiles"
cd "${HOME}/.config/dotfiles"
./setup.sh
source env
```

## Architecture

Project has a few core directories:

- **Project specific scripts:** Scripts located in `bin`. This is scripts intended for boot strapping and testing.
- **Reusable code:** Scripts in `lib` intended to be used as reusable code
- **Module directories:** This isolated things located in the `pkg` directory. Each should at least have the following
  - **Global binaries:** Will have exported scripts in a `bin` directory
  - **install.sh:** Installation script for the specific module. This should be idempotent
  - **env.sh:** This will be automatically sourced

### Modules

- **nvim:** My neovim setup
- **shell:** My current shell setup. This makes up the base of my setup
- **Docker:** TODO
- **go:** Golang things

## Installation testing

Make sure you are in the project directory

```bash
. bin/env # setup dev environment
docker run -it \
	--mount "type=bind,src=${PWD},dst=/root/.config/dotfiles" \
	ubuntu \
	"$@"
```
