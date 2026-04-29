# bash completion for mux (zellij session helper).
#
# Wired in by sourcing this file from .bashrc:
#   . "${XDG_CONFIG_HOME:-$HOME/.config}/mux/completions/mux.bash"

_mux() {
	local cur prev sub sessions
	cur="${COMP_WORDS[COMP_CWORD]}"
	prev="${COMP_WORDS[COMP_CWORD-1]}"
	sub="${COMP_WORDS[1]:-}"

	# Live session names. -F treats them as fixed strings and the IFS=$'\n'
	# split protects names that contain spaces or shell metacharacters.
	local IFS=$'\n'
	sessions=($(zellij ls --no-formatting --short 2>/dev/null))
	IFS=$' \t\n'

	case "$COMP_CWORD" in
	1)
		COMPREPLY=($(compgen -W "ls rm kill --help -h ${sessions[*]}" -- "$cur"))
		;;
	2)
		case "$sub" in
		rm | kill)
			COMPREPLY=($(compgen -W "${sessions[*]}" -- "$cur"))
			;;
		esac
		;;
	esac
}

complete -F _mux mux
