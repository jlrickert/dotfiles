# Common shell functions
alias tf='tail -f'
alias -g L="| less"
alias -g N="| /dev/null"
alias -g G="| grep -Ein"
alias top="htop"
alias free="free -h"
alias clip="xclip -se -c"

# Python
alias py="python"
alias py2="python2"

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
alias ll='ls -alGh --color=auto'
alias la='ll -A'
alias ls='ls -Gh --color=auto'

# Common Programming stuff
alias ctags='ctags -R . --exclude=target --exclude=vendor --exclude=project'

alias gdb="gdb --quiet"

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

