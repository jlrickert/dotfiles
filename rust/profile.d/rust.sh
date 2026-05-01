: "${XDG_DATA_HOME:=$HOME/.local/share}"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
case ":$PATH:" in
    *":$CARGO_HOME/bin:"*) ;;
    *) PATH="$CARGO_HOME/bin:$PATH" ;;
esac
export PATH
