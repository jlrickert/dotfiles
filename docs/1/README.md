# fzf installer

[fzf] installer does a couple of things:

-   Download binary from github if available. Otherwise, it builds it if go is
    setup
-   Optionally create a copy of shell libraries for xdg things
-   Optionally configure shell dotfiles

By default [fzf] recommends installing to `~/.fzf`. This could be any where. My
approach is to have the installer get the binary and add its libraries to
`~/.local/share/fzf` and `~/.config/fzf`. The binaries should then be symlinked
to `~/.local/bin`. Shell configuration is handled by us.

This is done by running the following:

```bash
cd deps/fzf
./install --no-update-rc --xdg
stow bin --target ~/.local/bin --verbose=2
```

`WARNING`: need to verify this next statement

The way that [fzf] handles config files is that it hard codes the paths in
`~/.local/config`. This leads to weird redundancy in `PATH`.

[fzf](https://github.com/junegunn/fzf)
