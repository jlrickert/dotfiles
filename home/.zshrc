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
  # fzf
  gh
  git
  golang
  node
  npm
  rust
  vi-mode
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

_have() { command -v "$1" &>/dev/null; }

# bonzai style completions
owncomp=(keg auth gocomplete chrome kn knp knw ku pomo qf sshkey vic)
for i in "${owncomp[@]}"; do _have "$i" && complete -C "$i" "$i"; done

_have flutter && source <(flutter zsh-completion)
_have podman && source <(podman completion zsh)
_have kubectl && source <(kubectl completion zsh)
_have hugo && source <(hugo completion zsh) && compdef _hugo hugo
_have k3s && source <(k3s completion zsh)
_have helm && source <(helm completion zsh)
_have fnm && source <(fnm env --use-on-cd)
_have fzf && source "${XDG_CONFIG_HOME:-$HOME/.config}/fzf/fzf.zsh"

# python argparse completions
_argparse() {
  # This is from the python3-argcomplete package on ubuntu
  # FIXME: make this work on other distros
  if _have register-python-argcomplete3 && _have "$1"; then
    source <(register-python-argcomplete3 "$1")
  fi
}

argcomp=(ansible ansible-config ansible-console ansible-doc ansible-galaxy ansible-inventory ansible-playbook ansible-pull ansible-vault dotctl)
for i in "${argcomp[@]}"; do _argparse "$i"; done

_have nvim && alias vim=nvim
_have ranger && alias r=ranger
_have exa && alias ls='exa --color=auto'
_have exa && alias la='exa -lah'
_have bat && alias cat=bat

#
# FZF setup
#
_fzf_compgen_path() {
  rg --files --hidden --color=never
}

_fzf_compgen_dir() {
  fdfind --color=never --type d --follow --exclude ".git"
}

_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    cd)           fzf --preview 'tree -C {} | head -200'           "$@" ;;
    export|unset) fzf --preview "eval 'echo \$'{}"                 "$@" ;;
    ssh)          fzf --preview 'dig {}'                           "$@" ;;
    keg)          ku fzf                                           "$@" ;;
    knp)          KEG_CURRENT=~/personal/zettel ku fzf             "$@" ;;
    knw)          KEG_CURRENT=~/work/zet ku fzf                    "$@" ;;
    kn)           KEG_CURRENT=~/personal/zet ku fzf                "$@" ;;
    chrome)       fzf --ansi --multi                               "$@" ;;
    *)            fuz                                              "$@" ;;
  esac
}

# TODO: figure out what is going on here.  This feeds an initial list of nodes
# to fzf to prevent garbage displaying at first
_fzf_complete_keg() {
  _fzf_complete +m -- "$@" < <(ku nodes)
}
_fzf_complete_knp() {
  _fzf_complete +m -- "$@" < <(KEG_CURRENT=~/personal/zettel ku nodes)
}
_fzf_complete_knw() {
   _fzf_complete +m -- "$@" < <(KEG_CURRENT=~/work/zet ku nodes)
}
_fzf_complete_kn() {
  _fzf_complete +m -- "$@" < <(KEG_CURRENT=~/personal/zet ku nodes)
}
_fzf_complete_chrome() {
  _fzf_complete +m -- "$@" < <(chrome history)
}
_fzf_complete_vic() {
  _fzf_complete +m -- "$@" < <(vic list)
}
