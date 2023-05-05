# My personal dotfiles

Pilfer at your own peril.

## Install Dependencies

### Ubuntu

```bash
sudo apt install -y python3-pip git
```

## Install

Run the following to quickly install.  This will also ask for a password on the command line.

```bash
git clone git@github.com:jlrickert/dotfiles.git
cd dotfiles
./hack
./install
./scripts/dotctl
```

`./hack` may be run before `./scripts/dotctl` to remove the password prompt.
