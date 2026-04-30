# Pin the `go env` file location to ~/.config/go/env on every platform.
# Without this, os.UserConfigDir() resolves to ~/Library/Application Support
# on macOS, so the file dots places at ~/.config/go/env would be ignored.
# https://go.dev/doc/go1.13#tools, https://pkg.go.dev/os#UserConfigDir
export GOENV="${XDG_CONFIG_HOME:-$HOME/.config}/go/env"

# Linux tarball install lands here; add it to PATH. macOS brew puts go on
# PATH via brew shellenv (already run in common-shell/profile), so this
# block is a no-op there.
_go_local="${XDG_DATA_HOME:-$HOME/.local/share}/go"
if [ -d "$_go_local/bin" ]; then
	case ":$PATH:" in
		*":$_go_local/bin:"*) ;;
		*) PATH="$_go_local/bin:$PATH" ;;
	esac
	export PATH
fi
unset _go_local

# `go install` destination. ~/.local/bin is on PATH from common-shell.
export GOBIN="$HOME/.local/bin"
