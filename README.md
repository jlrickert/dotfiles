# Dotfiles of Jared Rickert

My personal dotfiles. Pilfer at your own peril 😈

## Bootstrap a fresh machine

Install `dots` (see https://github.com/jlrickert/dots#installation), then:

```bash
dots init --from git@github.com:jlrickert/dotfiles.git --path dots-config --name jlrickert
```

Or skip the local install entirely and try the prebuilt container:

```bash
docker run -it --rm ghcr.io/jlrickert/dotfiles:ubuntu
```

## Testing

End-to-end installs are exercised in containers via [Task](https://taskfile.dev), Docker, and Docker Compose. From the repo root:

```bash
task build:ubuntu     # build the prebuilt image
task test             # build + run the e2e verify script in-container
task shell:ubuntu     # drop into the prebuilt container interactively
task push:ubuntu      # publish to ghcr.io/jlrickert/dotfiles:ubuntu (needs `task login`)
task clean            # remove built images and compose state
```

The verify script lives at `tests/e2e/verify.sh` and asserts that the expected symlinks, CLI tools, and shells all come up cleanly inside the image.
