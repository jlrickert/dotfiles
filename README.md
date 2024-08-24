# Dotfiles of Jared Rickert

My personal dotfiles. Pilfer at your own peril 😈

## Installation

### Ubuntu

```bash
sudo apt install -y git curl
sh -c "$(curl -fsLS https://raw.githubusercontent.com/jlrickert/dotfiles/main/setup.sh)"
```

### MacOSX

```bash
brew install curl git # I am guessing here. Too lazy to look it up
sh -c "$(curl -fsLS https://raw.githubusercontent.com/jlrickert/dotfiles/main/setup.sh)"
```

## Development

Use `pnpm`

## Testing

> [!NOTE]
>
> Requires docker

```bash
./scripts/docker-build.sh
./scripts/docker-start.sh
```

```bash
./repos/github.com/jlrickert/dotfiles/setup.sh
```
