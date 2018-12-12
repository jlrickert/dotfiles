########################################
## Environmental variables
########################################
export BROWSER=google-chrome-stable
export DEFAULT_USER=`whoami`
export EDITOR=vim
export LANG=en_US.UTF-8
export PAGER=less
export VISUAL=vim
export ZSH=$HOME/.oh-my-zsh
export CUSTOM_ZSH=$HOME/.local/share/zsh


########################################
## Paths
########################################
# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

path=(
    $HOME/.local/bin
    $path[@]
)

# Set the list were completions are stored
fpath=(
    $CUSTOM_ZSH/completions
    $fpath[@]
)

# Set the the list of directories that cd searches.
cdpath=(
    $cdpath
)


########################################
## OH MY ZSH configuration
########################################
ZSH_THEME="jlrickert"
ENABLE_CORRECTION="false"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="yyyy-mm-dd"
plugins=(
    common-aliases
    fasd
    git
    heroku
    jsontools
    vi-mode

    pip
)

source $CUSTOM_ZSH/init.zsh


########################################
## Custom configuration
########################################
for src in $CUSTOM_ZSH/plugins/*; do
    source $src
done