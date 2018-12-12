# Updates editor information when the keymap changes.
function zle-keymap-select() {
  zle reset-prompt
  zle -R
}

# Ensure that the prompt is redrawn when the terminal size changes.
TRAPWINCH() {
  zle &&  zle -R
}

zle -N zle-keymap-select
zle -N edit-command-line

bindkey -v

# allow v to edit the command line (standard behaviour)
autoload -Uz edit-command-line
bindkey -M vicmd 'v' edit-command-line

# if mode indicator wasn't setup by theme, define default
if [[ "$MODE_INDICATOR" == "" ]]; then
  MODE_INDICATOR="%{$fg_bold[red]%}<%{$fg[red]%}<<%{$reset_color%}"
fi

function vi_mode_prompt_info() {
  echo "${${KEYMAP/vicmd/$MODE_INDICATOR}/(main|viins)/}"
}

# define right prompt, if it wasn't defined by a theme
if [[ "$RPS1" == "" && "$RPROMPT" == "" ]]; then
  RPS1='$(vi_mode_prompt_info)'
fi

################################################################################
### Custom Keymaps
################################################################################
bindkey '^[p' up-history
bindkey '^[n' down-history
bindkey '^[r' history-incremental-search-backward
bindkey -M vicmd "w" vi-backward-word
bindkey -M vicmd "e" vi-forward-word-end
bindkey -M vicmd '^[n' down-history
bindkey -M vicmd '^[p' up-history
bindkey -M vicmd 'k' up-line
bindkey -M vicmd 'j' down-line
export KEYTIMEOUT=1
