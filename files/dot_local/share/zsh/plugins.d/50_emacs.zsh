EMACS_BIN=$(which emacs)
function emacs() {
	setsid "$EMACS_BIN" "$@"
}
