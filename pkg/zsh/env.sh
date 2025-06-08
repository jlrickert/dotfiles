if [ -z "${ZSH_VERSION+set}" ] || [ -z "$ZSH_VERSION" ]; then
	return 0
fi

## Stop if non interactive
case "$-" in
	*i*) ;;
	*) return 0 ;;
esac

autoload -Uz compinit
compinit

export FUNCNEST=10000 # Or a higher value if needed
have starship && eval "$(starship init zsh)"
have ssh-agen && eval "$(ssh-agen)" &>/dev/null
# shellcheck disable=SC1094
source "${PACKAGE_PATH}/lib/zsh-vi-mode.zsh"

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
