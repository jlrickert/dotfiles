# Changelog

All notable changes to this project are documented in this file.

## v0.19.0 - 2026-05-04



### 🐛 Bug Fixes
- add unzip to common-shell apt install list
- install neovim from upstream tarball pinned to a recent release


### 🚀 Features
- harden dots package installs across fresh and full-image bootstraps
- replace bun package with a unified javascript runtime package


### 🚜 Refactor
- drop empty top-level links stub from homebrew manifest


### 🧪 Testing
- probe ed --version to confirm cmd-edit owns the PATH


## v0.18.0 - 2026-05-02



### 🚀 Features
- migrate dotfiles to dots packages and remove legacy installer trees
- add python dots package with python3 + uv runtime
- **clone:** add gh-clone/bb-clone with supply-chain safety gate
- add jq and yq to common-shell toolkit
- add wezterm dots package with GUI terminal config


## v0.17.0 - 2026-04-30



### 🚀 Features
- **shell:** install dots completions in bash and zsh packages
- make zsh history reliable under terminal multiplexers


## v0.16.0 - 2026-04-30



### 🐛 Bug Fixes
- **docker:** make ubuntu image build cleanly under qemu (multi-arch)


### 🚀 Features
- **docker:** add full ubuntu image variant (zellij + go)


## v0.15.0 - 2026-04-30



### 🚀 Features
- **ci:** build multi-arch docker images (amd64 + arm64)


## v0.14.0 - 2026-04-30



### ⚙️ Miscellaneous
- update release system
- **neovim:** bump dependencies versions
- set VISUAL environment variable to nvim-beta in ku script
- add dots package manager configuration
- **dots-config:** drop hard-coded work_mode paths from shared config
- **docker:** bump dots to 0.6.0 and trim ubuntu install set


### 🐛 Bug Fixes
- neovim package version and documentation
- added missing docker entry point
- bootstrap script now works
- Rename `mux` to `muxi`
- Adjust Dockerfile copy and install order
- Correct package bin and env loading
- Ensure local binaries persist in Docker shell
- resolve keg detection issue by improving KEG_CURRENT assignment
- **shell:** install starship.toml so theme survives in fresh containers
- **zellij:** drop EXIT trap that referenced out-of-scope local var
- **ci:** merge release-publish into release to avoid GITHUB_TOKEN trigger gap


### 🚀 Features
- **neovim:** Add neovim package
- **chezmoi:** add chezmoi package
- **chezmoi:** update packages
- add install script for chezmoi
- add AI scripts for generating Git comments and enhancing functions
- **neovim:** update dependencies and add CSS snippets
- **chezmoi:** update configurations
- Add assistant subcommand feature to commentthis script
- enhance LLM integration and add utility scripts
- Enable containerized neovim development and local installation
- **shell:** Add a broad range of utility scripts
- **core:** Add isolated development environment with Docker
- **shell:** Add Zsh configuration and blockinfile usage
- **system:** Implement modular dotfile management
- **core:** Add package list subcommands
- **shell:** Integrate zsh-vi-mode and Starship
- **zsh:** Implement comprehensive bash completion
- **shell:** Add utilities and refactor completion
- **deno:** Improve Zsh cache and Deno install
- Implement Docker entrypoint workflow and XDG compliance
- Add function to install/update git packages
- **editor:** Add script for launching nvim-beta
- add fzf subcommand for interactive item selection and editing
- add generate-icons script to generate app icons
- add kcat shell helpers and improve kcat-file input
- support editing piped stdin in ku command
- add zellij configuration and theme switcher
- add codex package with zet-keg skill for knowledge graph integration
- add git-commit skill and improve codex skill guidance
- add Claude agent system with KEG integration and install script
- **claude:** migrate to tap CLI, add statusline script and new skills
- **codex:** migrate to tap CLI and add new skills
- add shell packages for the dots package manager
- add taskfile-driven docker testing and prebuilt ubuntu image
- **shell:** add bun support and stub-based zsh linking
- **docker:** set ubuntu container timezone to America/Chicago
- **shell:** migrate zsh package to full config with marker-based stub
- **zellij:** add zellij dots package with mux session helper
- **shell:** native mux completions via generic bash/zsh discovery dirs
- **shell:** add EDITOR/VISUAL defaults and profile.d drop dir
- **go:** add go dots package replacing legacy pkg/20_go
- **ci:** add tapper-style release flow with manual dispatch and tag-triggered publish


### 🚜 Refactor
- Rework core structure, installation, and build
- Improve shell setup and Zsh completion
- **core:** Restructure build, install, and shell setup
- Centralize env and restructure scripts/packages
- **zsh:** Restructure completion loading setup
- update shell scripts to unify AI API usage and improve context handling
- remove deprecated script and improve ku command functions
- streamline shell bin scripts and update utilities
- simplify git commit message generation with pagent role


### 🧪 Testing
- **e2e:** assert files instead of symlinks for installed dotfiles

