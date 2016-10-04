## Clipboard
# stolen from http://madebynathan.com/2011/10/04/a-nicer-way-to-use-xclip/
cb() {
    local _scs_col='\e[0;32m'
    local _wrn_col='\e[1;31m'
    local _trn_col='\e[0;33m'

    # Check that xclip is installed.
    if ! type xclip > /dev/null 2>&1;
    then
        echo -e "$_wrn_col""You must have the 'xclip' program installed.\e[0m"
        # Check user is not root (root doesn't have access to user xorg server)
    elif [[ "$USER" == "root" ]]; then
        echo -e "$_wrn_col""Must be regular user (not root) to copy a file to the clipboard.\e[0m"
    else
        # If no tty, data should be available on stdin
        if ! [[ "$( tty )" == /dev/* ]];
        then
            input="$(< /dev/stdin)"
            # Else, fetch input from params
        else
            input="$*"
        fi

        # If no input, print usage message.
        if [ -z "$input" ];
        then
            echo "Copies a string to the clipboard."
            echo "Usage: cb <string>"
            echo "       echo <string> | cb"
        else
            # Copy input to clipboard
            echo -n "$input" | xclip -selection c
            # Truncate text for status
            if [ ${#input} -gt 80 ];
            then
                # echo -n "$input"
                input="$(echo "[master]" | cut -c1-80)$_trn_col...\e[0m";
            fi
            # Print status.
            echo -e "$_scs_col""Copied to clipboard:\e[0m $input"
        fi
    fi
}

function cbf() { cat "$1" | cb; }

## Haskell
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

function eecho {
    echo $@ 1>&2
}

function fire_fox_open {
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
