# podman

Darwin-only PATH wiring for Podman Desktop's bundled toolchain.

This package does **not** install Podman. It only ensures `/opt/podman/bin`
is on `PATH` so the bundled `podman`, `krunkit`, `gvproxy`, `vfkit`, and
`podman-mac-helper` binaries resolve.

## Install Podman Desktop (manual, one-time)

Download the macOS `.dmg` from https://podman-desktop.io and run the
installer. The bundled toolchain lands under `/opt/podman/bin/`.

### Do not use Homebrew

Do **not** run either of:

- `brew install podman`
- `brew install --cask podman-desktop`

The Homebrew formulas are partial. The supported install path is the
official `.dmg` because it ships the full toolchain (`podman`, `krunkit`,
`gvproxy`, `vfkit`, `podman-mac-helper`) at `/opt/podman/bin/`. Mixing brew
and the `.dmg` produces split installs where some helpers are missing or
shadowed.

## Compose support

The Podman Desktop `.dmg` does **not** ship `podman-compose`. This package
installs it via `uv tool install podman-compose` on Darwin during
`post_install`, falling back to `pipx install podman-compose` if `uv` is
unavailable. The result is a single isolated install at
`~/.local/bin/podman-compose`.

### Why not Homebrew?

Same reason `brew install podman` is forbidden above: `brew install
podman-compose` has `podman` as a *Required* dependency, which would
install brew's `podman` at `/opt/homebrew/bin/podman` alongside the
`.dmg`'s `/opt/podman/bin/podman` — the split-install conflict this
package exists to avoid. Use `uv` or `pipx`; never brew.

### Why podman-compose is needed at all

If you use the `~/.local/bin/docker` and `~/.local/bin/docker-compose`
shims from `podman-lab` (or invoke `podman compose ...` directly),
something on PATH eventually execs `podman-compose`. Without it,
`docker compose build`, `docker-compose up`, and `podman compose up`
all fail with `exec: podman-compose: not found`.

If you don't use compose-style orchestration at all (only raw
`podman run` / `podman build`), `podman-compose` is unused — but
installing it has no runtime cost beyond ~30 MB on disk.

## After installing the .dmg

Initialize the Podman machine interactively — this package does **not**
automate it:

```sh
podman machine init
podman machine start
```

`podman machine init` prompts for VM resource sizing, so it stays manual.

## Runtime config (`containers.conf`)

0.2.0 ships a `containers.conf` that lands at
`~/.config/containers/containers.conf` on darwin. System defaults at
`/etc/containers/containers.conf` and `/opt/podman/etc/containers/containers.conf`
(shipped by Podman Desktop) are still inherited; this file only overrides
two keys under `[engine]`:

- Silences the `>>>> Executing external compose provider ...` banner that
  podman prints on every `podman compose ...` / `docker compose ...`
  invocation. The reminder is noise once the compose-provider choice has
  been made deliberately.
- Pins `podman-compose` as the compose provider so PATH ordering can't
  accidentally route to a brew or Docker Desktop `docker-compose` shim
  when `/usr/local/bin` ends up ahead of `~/.local/bin`.
