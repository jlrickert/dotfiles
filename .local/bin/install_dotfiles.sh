git clone --bare git@github.com:jlrickert/dotfiles.git $HOME/.dotfiles

function config {
    /usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME $@
}

mkdir -p .config-backup

config checkout

if [ $? = 0 ]; then
    echo "Checked out config.";
else
    echo "Backing up prexisting dotfiles.";
    config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} .config-backup/{}
fi

# attempt installing again
config checkout

config config status.showUntrackedFiles no

