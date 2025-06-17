pathappend "${XDG_DATA_HOME}/deno/bin"

if [ ! -r "${DOTFILES_CACHE_HOME}/zsh/completions/_deno" ]; then
	mkdir -p "${DOTFILES_CACHE_HOME}/zsh/completions"
	deno completions zsh >"${DOTFILES_CACHE_HOME}/zsh/completions/_deno"
fi
