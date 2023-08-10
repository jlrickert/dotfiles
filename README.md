# My personal dotfiles

My personal dotfiles. Pilfer at your own peril!

## Installation

Installation is simple. Just install git and curl, copy over the chezmoi config, and let chezmoi do the rest. Sudo access is assumed.

Start by copying the following and updating.

```toml
[data]
    chezmoi_managed = "Managed by chezmoi"
    name = "Your name"
    email = "example@gmail.com"
    openai_token = ""
    discord_token = ""
    workstation = false # TODO: unstable. This is for setting up things like i3.
    is_wsl = false
    is_macos = false
    is_linux = true
    is_ubuntu = true
```

Supported OS currently are WSL2 and MacOSX. Optionally `--branch vX.X.X` may be specified to lock in a version (untested).

### Ubuntu and WSL2

```bash
sudo apt install -y git curl
$ sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply jlrickert
```

### MacOSX

```bash
brew install curl git # I am guessing here. Too lazy to look it up
$ sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply jlrickert
```

## Architecture

All things managed by chezmoi are placed in `home`.

| Directory | Description                    |
| --------- | ------------------------------ |
| home      | my dotfiles managed by chezmoi |

## Installation steps

WIP

1. Run `./install`. This will install needed dependencies
2. Run `./setup`

## Quick testing cheatsheet

Spin up a dev environment with `podman` to test in isolation. Optionally, `podman` may be replaced by `docker`.

```
podman run --rm -it -v `pwd`:/home/jlrickert/.local/share/chezmoi ubuntu bash
```

The following commands are run inside of the container

| Command           | Description                       |
| ----------------- | --------------------------------- |
| `chezmoi apply`   | Run installation                  |
| `chezmoi upgrade` | Pull latest changes from upstream |
