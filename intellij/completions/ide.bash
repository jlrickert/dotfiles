# bash completion for ide (IntelliJ IDEA launcher).
#
# Wired in by sourcing this file from .bashrc:
#   . "${XDG_CONFIG_HOME:-$HOME/.config}/bash/completions/ide.bash"

_ide() {
	local cur
	cur="${COMP_WORDS[COMP_CWORD]}"

	if [ "$COMP_CWORD" -eq 1 ]; then
		COMPREPLY=($(compgen -W "help --help -h" -- "$cur"))
		case "$cur" in
		/* | .* | ~* | "")
			COMPREPLY+=($(compgen -f -- "$cur"))
			;;
		esac
	fi
}

complete -F _ide ide
