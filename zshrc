if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

###############################################################################
# Aliases
###############################################################################
# Common shell functions
alias -g G="| grep -Ein"
alias -g L="| less"
alias -g N="| /dev/null"
alias -g H='| head'
alias -g T='| tail'
alias -g Y='| xclip && xclip -o | xclip -se c'

alias top="htop"
alias free="free -h"
alias clip="xclip -se -c"

# jump to recently used items
alias a='fasd -a'        # any
alias s='fasd -si'       # show / search / select
alias d='fasd -d'        # directory
alias f='fasd -f'        # file
alias sd='fasd -sid'     # interactive directory selection
alias sf='fasd -sif'     # interactive file selection
alias z='fasd_cd -d'     # cd, same functionality as j in autojump
alias zz='fasd_cd -d -i' # cd with interactive selection

# Show human friendly numbers and colors
alias df='df -h'
alias du='du -h -d 2'
alias ll='ls -alh --color=auto'
alias la='ll -A'
alias ls='ls -h --color=auto'

# Common Programming stuff
alias ctags='ctags -R . --exclude=target --exclude=vendor --exclude=project'

alias gdb="gdb --quiet"

# View HTTP traffic
alias sniff="sudo ngrep -d 'en0' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i en0 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

# Gzip-enabled `curl`
alias gurl="curl --compressed"

###############################################################################
# Go settup
###############################################################################
export GOPATH=~/Development/gocode
path=(
$path
$GOPATH/bin
)

###############################################################################
# Haskell setup
###############################################################################
function cabal_sandbox_info() {
  cabal_files=(*.cabal(N))
  if [ $#cabal_files -gt 0 ]; then
    if [ -f cabal.sandbox.config ]; then
      echo "%{$fg[green]%}sandboxed%{$reset_color%}"
    else
      echo "%{$fg[red]%}not sandboxed%{$reset_color%}"
    fi
  fi
}

###############################################################################
# Android
###############################################################################
export ANDROID_HOME=/opt/android-sdk

###############################################################################
# Nix
###############################################################################
if [ -e /home/raviko/.nix-profile/etc/profile.d/nix.sh ]; then
  . /home/raviko/.nix-profile/etc/profile.d/nix.sh;
fi # added by Nix installer

# RPROMPT="\$(cabal_sandbox_info) $RPROMPT"
###############################################################################
# Keymap
###############################################################################
bindkey '^[n' down-history
bindkey '^[p' up-history
bindkey '^[r' history-incremental-search-backward
bindkey -M vicmd "w" vi-backward-word
bindkey -M vicmd "e" vi-forward-word-end
export KEYTIMEOUT=1

###############################################################################
# dir colors
###############################################################################
eval `dircolors $HOME/.dircolors`

FILE=/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
if [ -f $FILE ]; then
  source $FILE
fi

###############################################################################
# Useful functions
###############################################################################
function eecho {
  echo $@ 1>&2
}

function ff {
  if [ $# = 0 ]; then
    eecho "usage: ff <file>" 1>&2
    return 1
  fi
  if [ -d "$1" ]; then
    eecho "That's a directory, dumbass." 1>&2
    return 1
  elif [ "${1%/*}" = "$1" ]; then
    firefox -new-tab "file://$(pwd)/$1"
  else
    "cd" "${1%/*}"
    local dir="$(pwd)"
    "cd" - >/dev/null
    firefox -new-tab "file://$dir/${1##*/}"
  fi
  return 0
}

###############################################################################
# Ruby stuff
###############################################################################
# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# Add RVM to PATH for scripting
path=(
$path
$HOME/.rvm/bin # rvm needs to be first
)
###############################################################################
# Python
###############################################################################
# setup pyenv with those 2 commands
#   git clone git://github.com/yyuu/pyenv.git ~/.pyenv
#   git clone https://github.com/yyuu/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv

# pyenv root
export PYENV_ROOT="$HOME/.pyenv"

# Add pyenv root to PATH
# and initialize pyenv
if [[ -d $PYENV_ROOT ]];then
  PATH="$PYENV_ROOT/bin:$PATH"
  # initialize pyenv
  eval "$(pyenv init -)"
  # initialize pyenv virtualenv
  eval "$(pyenv virtualenv-init -)"
fi

alias py="python"
alias py2="python2"
