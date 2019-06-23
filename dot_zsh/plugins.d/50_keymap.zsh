export KEYTIMEOUT=1
bindkey '^[p' up-history
bindkey '^[n' down-history
bindkey '^[r' history-incremental-search-backward
bindkey -M vicmd "w" vi-backward-word
bindkey -M vicmd "e" vi-forward-word-end
bindkey -M vicmd '^[n' down-history
bindkey -M vicmd '^[p' up-history
bindkey -M vicmd 'k' up-line
bindkey -M vicmd 'j' down-line
