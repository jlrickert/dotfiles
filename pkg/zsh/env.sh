if [ -z "${ZSH_VERSION+set}" ] || [ -z "$ZSH_VERSION" ]; then
	return 0
fi

autoload -Uz compinit
compinit

have starship && eval "$(starship init zsh)"

bindkey '^k' up-history
bindkey '^j' down-history

if have brew; then
	HOMEBREW_PREFIX="$(brew --prefix)"
	if [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]]; then
		# shellcheck disable=SC1091
		source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
	else
		for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*; do
			# shellcheck disable=SC1090
			[[ -r "${COMPLETION}" ]] && source "${COMPLETION}"
		done
	fi
fi

if have ssh-agent; then
	eval "$(ssh-agent)" &>/dev/null
fi
