# Path to your oh-my-zsh installation.

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="agnoster"
plugins=(
  ag
  command-not-found
  cp
  debian
  direnv
  fzf
  gh
  git
  golang
  node
  npm
  rust
  vi-mode
  fnm
  vscode
  z
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
#if [[ -n $SSH_CONNECTION ]]; then
#  export EDITOR='vim'
#else
#  export EDITOR='nvim'
#fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# _source_if ~/.zsh_profile

alias luamake=/home/jlrickert/code/sumneko/3rd/luamake/luamake

################################################################################
#                      personalized configuration
################################################################################

[[ -f "$HOME/.secrets" ]] && source "$HOME/.secrets"
[[ -f "$HOME/.work-secrets" ]] && source "$HOME/.work-secrets"

# tabtab source for packages
# uninstall by removing these lines
[[ -f ~/.config/tabtab/zsh/__tabtab.zsh ]] && . ~/.config/tabtab/zsh/__tabtab.zsh || true

command -v keg &>/dev/null && complete -C keg keg
command -v pomo &>/dev/null && complete -C pomo pomo
command -v podman &>/dev/null && source <(podman completion zsh)
command -v kubectl &>/dev/null && source <(kubectl completion zsh)
command -v k3s &>/dev/null && source <(k3s completion zsh)
command -v helm &>/dev/null && source <(helm completion zsh)
command -v hugo &>/dev/null && source <(hugo completion zsh) && compdef _hugo hugo
command -v fnm &>/dev/null && source <(fnm env --use-on-cd)
alias vim=nvim

