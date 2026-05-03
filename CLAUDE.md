# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repo status: mid-migration to `dots`

This repo is being migrated from two legacy layouts to the `dots` package manager (https://github.com/jlrickert/dots, tracked as keg:dev/141). The migration is in progress — both old and new layouts coexist. **Don't add new content to the legacy directories.**

| Status | Directories | Notes |
| --- | --- | --- |
| **Current** (dots packages) | `dots-config/`, `bash/`, `zsh/`, `common-shell/`, `zellij/`, `go/`, `claude/`, `codex/`, `editor/`, `knut/`, `rust/`, `homebrew/`, `javascript/`, `wezterm/` | Each has a `Dotfile.yaml` manifest plus config files |

When adding new functionality, create a new top-level `<name>/` package directory with a `Dotfile.yaml`.

## Dots package shape

A package is a top-level directory containing:

- `Dotfile.yaml` — the manifest (always opens with the `# yaml-language-server: $schema=…/schemas/dotfile.json` modeline)
- The actual config files (must be valid standalone — there is no template language)
- Optional `hooks/` with `install-*.sh`, `uninstall-*.sh` scripts referenced by the manifest

Manifest blocks in active use here: `package:` (name/version/description/tags/platforms/requires), `links:` (source-relative → target with path aliases), `platform:` (per-OS overrides, e.g. `darwin:` for the launchd plist in `zellij/`), `hooks:` (`post_install`, `pre_remove`).

**Path aliases** in `links:`/`platform:` resolve to platform-native paths: `@config` → `~/.config/`, `@home` → `~/`, `@bin` → `~/.local/bin/`. Bare paths like `.bashrc` mean home-relative.

**`requires: ["@self/<pkg>"]`** means same-tap dependency. Both `bash/` and `zsh/` require `@self/common-shell`.

**Marker-block pattern for installer-touched files.** `~/.zshrc` and `~/.zshenv` are deliberately _not_ in `zsh/Dotfile.yaml` `links:` — replacing them clobbers append-installs from bun/fnm/conda/etc. The pattern: real config lives at `@config/zsh/`, and a `post_install` hook injects a `BEGIN dotfiles-zsh` marker block that sources it. Apply the same pattern to any new file that third-party installers mutate. The e2e verify asserts the marker block (`tests/e2e/verify.sh`).

## `dots-config/` is the bootstrap package

`dots-config/Dotfile.yaml` is the meta-package that places `dots-config/config.yaml` at `~/.config/dots/config.yaml`. This is what bootstraps a fresh machine:

```bash
dots init --from git@github.com:jlrickert/dotfiles.git --path dots-config --name jlrickert
```

`dots-config/config.yaml` sets repo-wide defaults: `link_strategy: copy` (installed dotfiles are real files, not symlinks — the e2e verify relies on this), `backup: true`, `conflict_strategy: prompt`, `git.protocol: ssh`.

## Build / test / run

End-to-end installs run inside Ubuntu containers via [Task](https://taskfile.dev) + Docker Compose. From repo root:

```bash
task build:ubuntu      # build the prebuilt image (docker/ubuntu/Dockerfile)
task test              # build + run tests/e2e/verify.sh in-container
task shell:ubuntu      # interactive shell in the image
task push:ubuntu       # publish to ghcr.io/jlrickert/dotfiles:ubuntu (gated on clean tree)
task clean             # remove images + BuildKit cache (next build is from scratch)
```

`task push` refuses to push if the working tree is dirty so the SHA tag stays meaningful.

The verify script (`tests/e2e/verify.sh`) is the source of truth for "did the install succeed" — it asserts symlinks/files land at expected paths, CLI tools resolve, both bash and zsh load cleanly, and the zsh marker block is in place. When you add a new package, extend `verify.sh` with assertions for its outputs.

## Docker layout

End-to-end testing uses two images, both built from `docker/ubuntu/`:

- `docker/ubuntu/Dockerfile` — slim image (common-shell + bash + zsh)
- `docker/ubuntu/Dockerfile.full` — full image (slim + zellij + go)

`docker-compose.yml` and `Taskfile.yml` reference these directly. There are no root-level Dockerfiles.

## Pending rename

`dots` → `dottap` is a pending rename (keg:dev/270 decision, keg:dev/271 task — both still open). The binary, schema URLs, and KEG notes still say `dots`. Don't pre-emptively rename anything.

## Working with KEG context

This repo's working directory resolves to keg alias `dev`. The migration plan and design background live in keg:dev — relevant nodes:

- dev/141 — Project: dots (full design, CLI table, data model)
- dev/273 — Project: dotfiles (this repo)
- dev/888 / dev/889 — Task + Plan: Modernize dotfiles (the migration checklist)
- dev/1011 — Decision: `--name`, copy-default, work-mode symlinks (v0.4.0 UX)
- dev/1091 — Issue: `dots init --from file://` doesn't see uncommitted files (Docker bootstrap workaround: snapshot-commit before init)
