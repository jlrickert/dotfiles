#!/bin/bash
# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# don't do anything of running interactively
case $- in
*i*) ;; #interactive
*) return ;;
esac

################################################################################
#                            Utility function
################################################################################

_have() { type "$1" &>/dev/null; }
_source_if() { [[ -r "$1" ]] && source "$1"; }

################################################################################
#                         environment variables
#                           (also see envx)
################################################################################
export USER="${USER:-$(whoami)}"
export GITUSER="$USER"
export REPOS="$HOME/Repos"
export GHREPOS="$REPOS/github.com/$GITUSER"
export DOTFILES="$HOME/personal/dotfiles"
export SCRIPTS="$HOME/scripts"
export SNIPPETS="$DOTFILES/snippets"
export HELP_BROWSER=lynx
export DESKTOP="$HOME/Desktop"
export DOCUMENTS="$HOME/Documents"
export DOWNLOADS="$HOME/Downloads"

export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:="$HOME/.cache"}
export XDG_STATE_HOME=${XDG_STATE_HOME:="$HOME/.local/state"}

export TEMPLATES="$HOME/Templates"
export PUBLIC="$HOME/Public"
export PRIVATE="$HOME/Private"
export PICTURES="$HOME/Pictures"
export MUSIC="$HOME/Music"
export VIDEOS="$HOME/Videos"
export ZETDIR="$GHREPOS/zet"
export TERM=xterm-256color
export HRULEWIDTH=73
_have nano && export EDITOR=nano
_have vi && export EDITOR=vi
_have vim && export EDITOR=vim
_have nvim && export EDITOR=nvim
_have vim && export VISUAL=vim
_have nvim && export VISUAL=nvim
_have vim && export EDITOR_PREFIX=vim
export NVM_DIR="$HOME/.local/share/nvm"
export RUSTUP_HOME="$HOME/.local/share/rustup"
export CARGO_HOME="$HOME/.local/share/cargo"
export GOPRIVATE="github.com/$GITUSER/*,gitlab.com/$GITUSER/*"
export GOPATH="$HOME/.local/share/go"
export GOBIN="$HOME/.local/bin"
#export GOPROXY=direct
export CGO_ENABLED=0
export PYTHONDONTWRITEBYTECODE=2
export LC_COLLATE=C
export ZSH="$XDG_DATA_HOME/oh-my-zsh"
#export CFLAGS="-Wall -Wextra -Werror -O0 -g -fsanitize=address -fno-omit-frame-pointer -finstrument-functions"

export LESS="-R"
#export PAGER=""
export MANPAGER="less -R --use-color -Dd+r -Du+b"

export LESS_TERMCAP_mb=$(
    tput bold
    tput setaf 2
) # green
export LESS_TERMCAP_md=$(
    tput bold
    tput setaf 6
) # cyan
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(
    tput bold
    tput setaf 3
    tput setab 4
) # yellow on blue
export LESS_TERMCAP_se=$(
    tput rmso
    tput sgr0
)
export LESS_TERMCAP_us=$(
    tput smul
    tput bold
    tput setaf 7
) # white
export LESS_TERMCAP_ue=$(
    tput rmul
    tput sgr0
)
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)
export GROFF_NO_SGR=1 # For Konsole and Gnome-terminal

################################################################################
#                                  GPG
################################################################################

export GPG_TTY=$(tty)

################################################################################
#                                 pager
################################################################################

if [[ -x /usr/bin/lesspipe ]]; then
    export LESSOPEN="| /usr/bin/lesspipe %s"
    export LESSCLOSE="/usr/bin/lesspipe %s %s"
fi

################################################################################
#                               dircolors
################################################################################

if _have dircolors; then
    if [[ -r "$HOME/.dircolors" ]]; then
        eval "$(dircolors -b "$HOME/.dircolors")"
    else
        eval "$(dircolors -b)"
    fi
fi

################################################################################
#                                 path
################################################################################

pathappend() {
    declare arg
    for arg in "$@"; do
        test -d "$arg" || continue
        PATH=${PATH//":$arg:"/:}
        PATH=${PATH/#"$arg:"/}
        PATH=${PATH/%":$arg"/}
        export PATH="${PATH:+"$PATH:"}$arg"
    done
} && export -f pathappend

pathprepend() {
    for arg in "$@"; do
        test -d "$arg" || continue
        PATH=${PATH//:"$arg:"/:}
        PATH=${PATH/#"$arg:"/}
        PATH=${PATH/%":$arg"/}
        export PATH="$arg${PATH:+":${PATH}"}"
    done
} && export -f pathprepend

# remember last arg will be first in path
pathprepend \
    "$HOME/.local/bin" \
    "$GHREPOS/cmd-"* \
    "$CARGO_HOME/bin" \
    /usr/local/go/bin \
    /usr/local/bin \
    "$SCRIPTS"

pathappend \
    /usr/local/bin \
    /usr/local/sbin \
    /usr/local/games \
    /usr/games \
    /usr/sbin \
    /usr/bin \
    /snap/bin \
    /sbin \
    /bin

################################################################################
#                                cdpath
################################################################################

export CDPATH=".:$GHREPOS:$DOTFILES:$REPOS:/media/$USER:$HOME"

################################################################################
#                          bash shell options
################################################################################

# shopt is for BASHOPTS, set is for SHELLOPTS

shopt -s checkwinsize # enables $COLUMNS and $ROWS
shopt -s expand_aliases
shopt -s globstar
shopt -s dotglob
shopt -s extglob

#shopt -s nullglob # bug kills completion for some
set -o noclobber

################################################################################
#                                history
################################################################################

export HISTCONTROL=ignoreboth
export HISTSIZE=5000
export HISTFILESIZE=10000

set -o vi
shopt -s histappend

################################################################################
#                             smart prompt
#                 (keeping in bashrc for portability)
################################################################################

PROMPT_LONG=20
PROMPT_MAX=95
PROMPT_AT=@

__ps1() {
    local P='$' dir="${PWD##*/}" B countme short long double \
        r='\[\e[31m\]' g='\[\e[32m\]' h='\[\e[34m\]' \
        u='\[\e[33m\]' p='\[\e[34m\]' w='\[\e[35m\]'

    b='\[\e[36m\]' x='\[\e[0m\]'

    [[ $EUID == 0 ]] && P='#' && u=$r && p=$u # root
    [[ $PWD = / ]] && dir=/
    [[ $PWD = "$HOME" ]] && dir='~'

    B=$(git branch --show-current 2>/dev/null)
    [[ $dir = "$B" ]] && B=.
    countme="$USER$PROMPT_AT$(hostname):$dir($B)\$ "

    [[ $B == master || $B == main ]] && b="$r"
    [[ -n "$B" ]] && B="$g($b$B$g)"

    short="$u\u$g$PROMPT_AT$h\h$g:$w$dir$B$p$P$x "
    long="$g╔ $u\u$g$PROMPT_AT$h\h$g:$w$dir$B\n$g╚ $p$P$x "
    double="$g╔ $u\u$g$PROMPT_AT$h\h$g:$w$dir\n$g║ $B\n$g╚ $p$P$x "

    if ((${#countme} > PROMPT_MAX)); then
        PS1="$double"
    elif ((${#countme} > PROMPT_LONG)); then
        PS1="$long"
    else
        PS1="$short"
    fi
}

PROMPT_COMMAND="__ps1"

################################################################################
#                               keyboard
################################################################################

# only works if you have X and are using graphic Linux desktop

#_have setxkbmap && test -n "$DISPLAY" && \
#  setxkbmap -option caps:escape &>/dev/null

################################################################################
#                                aliases
#      (use exec scripts instead, which work from vim and subprocs)
################################################################################

unalias -a
alias ip='ip -c'
alias '?'=duck
alias '??'=google
alias '???'=bing
alias dot='cd $DOTFILES'
alias scripts='cd $SCRIPTS'
alias snippets='cd $SNIPPETS'
alias ls='ls -h --color=auto'
alias la="ls -lAh --color=auto"
alias free='free -h'
alias tree='tree -a'
alias df='df -h'
alias chmox='chmod +x'
alias diff='diff --color'
alias sshh='sshpass -f $HOME/.sshpass ssh '
alias temp='cd $(mktemp -d)'
alias view='vi -R' # which is usually linked to vim
alias clear='printf "\e[H\e[2J"'
alias c='printf "\e[H\e[2J"'
alias iam=live
alias more='less'
alias '..'='cd ../'
alias '...'='cd ../../'
alias '....'='cd ../../../'

_have vim && alias vi=vim
_have nvim && alias vi=nvim
_have nvim && alias vim=nvim
_have ranger && alias r=ranger
_have exa && alias ls='exa --color=auto'
_have exa && alias la='exa -lah'
_have bat && alias cat=bat

################################################################################
#                               functions
################################################################################

# lesscoloroff() {
#   while IFS= read -r line; do
#     unset ${line%%=*}
#   done < <(env | grep LESS_TERM)
# } && export -f lesscoloroff

envx() {
    local envfile="${1:-"$HOME/.env"}"
    [[ ! -e "$envfile" ]] && echo "$envfile not found" && return 1
    while IFS= read -r line; do
        name=${line%%=*}
        value=${line#*=}
        [[ -z "${name}" || $name =~ ^# ]] && continue
        export "$name"="$value"
    done <"$envfile"
} && export -f envx

[[ -e "$HOME/.env" ]] && envx "$HOME/.env"

new-from() {
    local template="$1"
    local name="$2"
    ! _have gh && echo "gh command not found" && return 1
    [[ -z "$name" ]] && echo "usage: $0 <name>" && return 1
    [[ -z "$GHREPOS" ]] && echo "GHREPOS not set" && return 1
    [[ ! -d "$GHREPOS" ]] && echo "Not found: $GHREPOS" && return 1
    cd "$GHREPOS" || return 1
    [[ -e "$name" ]] && echo "exists: $name" && return 1
    gh repo create -p "$template" --public "$name"
    gh repo clone "$name"
    cd "$name" || return 1
}

new-bonzai() { new-from jlrickert/bonzai-example "$1"; }
new-cmd() { new-from jlrickert/template-bash-command "cmd-$1"; }

export -f new-from new-bonzai new-cmd

clone() {
    local repo="$1" user
    local repo="${repo#https://github.com/}"
    local repo="${repo#git@github.com:}"
    if [[ $repo =~ / ]]; then
        user="${repo%%/*}"
    else
        user="$GITUSER"
        [[ -z "$user" ]] && user="$USER"
    fi
    local name="${repo##*/}"
    local userd="$REPOS/github.com/$user"
    local path="$userd/$name"
    [[ -d "$path" ]] && cd "$path" && return
    mkdir -p "$userd"
    cd "$userd"
    echo gh repo clone "$user/$name" -- --recurse-submodule
    gh repo clone "$user/$name" -- --recurse-submodule
    cd "$name"
} && export -f clone

################################################################################
#               programming language specific source files
################################################################################

# TODO: this takes like 2 seconds too long to load
_source_if "$NVM_DIR/nvm.sh"

################################################################################
#               source external dependencies / completion
################################################################################

# for mac
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

owncomp=(keg auth gocomplete chrome kn knp knw ku pomo qf sshkey)
for i in "${owncomp[@]}"; do complete -C "$i" "$i"; done

_have gh && . <(gh completion -s bash)
_have pandoc && . <(pandoc --bash-completion)
_have spotify && . <(spotify completion bash 2>/dev/null)
_have podman && _source_if "$HOME/.local/share/podman/completion"
_have docker && _source_if "$HOME/.local/share/docker/completion"
_have docker-compose && complete -F _docker_compose dc          
_have fnm && source <(fnm env --use-on-cd)
# _have nvm && _source_if "$NVM_DIR/bash_completion"
_have fzf && source "$HOME/.config/fzf/fzf.bash"
_have cargo && _source_if "$CARGO_HOME/env"
_have terraform && complete -C /usr/bin/terraform terraform
_have terraform && complete -C /usr/bin/terraform tf

_have ansible && . <(register-python-argcomplete3 ansible)
_have ansible-config && . <(register-python-argcomplete3 ansible-config)
_have ansible-console && . <(register-python-argcomplete3 ansible-console)
_have ansible-doc && . <(register-python-argcomplete3 ansible-doc)
_have ansible-galaxy && . <(register-python-argcomplete3 ansible-galaxy)
_have ansible-inventory && . <(register-python-argcomplete3 ansible-inventory)
_have ansible-playbook && . <(register-python-argcomplete3 ansible-playbook)
_have ansible-pull && . <(register-python-argcomplete3 ansible-pull)
_have ansible-vault && . <(register-python-argcomplete3 ansible-vault)
#_have ssh-agent && test -z "$SSH_AGENT_PID" && . <(ssh-agent)

################################################################################
#                      personalized configuration
################################################################################

_source_if "$HOME/.bash-personal"
_source_if "$HOME/.bash-work"
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
