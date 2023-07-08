# My personal dotfiles

Pilfer at your own peril.

## Install Dependencies

### Ubuntu

```bash
sudo apt install -y python3-pip git
```

## Install

Run the following to quickly install. This will also ask for a password on the command line.

```bash
git clone git@github.com:jlrickert/dotfiles.git
cd dotfiles
./hack
./install
./scripts/dotctl
```

`./hack` may be run before `./scripts/dotctl` to remove the password prompt.

## File structure

| Directory | Description                                 |
| --------- | ------------------------------------------- |
| home      | my dotfiles managed by stow                 |
| deps      | third party repos managed by git submodules |

## Quick install

```bash
# install chezmoi if you don't have it already
sh -c "$(curl -fsLS get.chezmoi.io)" -- -b $HOME/.local/bin
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply git@github.com:jlrickert/dotfiles.git
```

## Installation steps

WIP

1. Run `./install`. This will install needed dependencies
2. Run `./setup`

## Test environment setup for quick prototyping

```
podman run --rm -it -v `pwd`:/home/jlrickert ubuntu bash
```
