#
# Executes commands at login post-zshrc.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Execute code that does not affect the current session in the background.
{
  # Compile the completion dump to increase startup speed.
  zcompdump="${ZDOTDIR:-$HOME}/.zcompdump"
  if [[ -s "$zcompdump" && (! -s "${zcompdump}.zwc" || "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
    zcompile "$zcompdump"
  fi
} &!

# Print a random, hopefully interesting, adage.
if (( $+commands[fortune] )); then
  if [[ -t 0 || -t 1 ]]; then
    fortune -s
    print
  fi
fi

# If Keychain is installed, run it.
hash keychain 2> /dev/null
if [ $? -eq 0 ]; then
    keychain -q ~/.ssh/id_rsa_raviko --agents ssh
    keychain -q ~/.ssh/hyperion --agents ssh
    if [ -f ~/.keychain/$HOST-sh ]; then
        source ~/.keychain/$HOST-sh
    fi
fi

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
