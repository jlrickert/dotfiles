typeset -U path

path=(
    /usr/local/{bin,sbin}
    $path[@]
    $HOME/.local/bin
)

export DOTFILES=$HOME/.dotfiles

if [[ "$SHLVL" -eq 4 && ! -o LOGIN && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
    source "${ZDOTDIR:-$HOME}/.zprofile"
fi
