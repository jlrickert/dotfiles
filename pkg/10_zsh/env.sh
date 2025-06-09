# Only run if zsh is the shell
if [ -z "${ZSH_VERSION+set}" ] || [ -z "$ZSH_VERSION" ]; then
	return 0
fi

########################################################################
# Interactive & Login setup
########################################################################
# Stop if non interactive
case "$-" in
*i*) ;;
*) return 0 ;;
esac

########################################################################
# Setup completion
########################################################################
#  Loads shell completions for executables found in the dotfiles binary
#  directory and installed package binary directories.
#
# Side Effects: Modifies the shell's completion system by adding completion
#               rules using the `complete` command for discovered executables.
function load_completions() {
	local executable_path=
	# Add completion for executables related to the core public dotfile
	# executables
	find "${DOTFILES_BIN}" -maxdepth 1 -type f -print0 | while IFS= read -r -d '' executable_path; do
		# Extract the basename of the executable file. This is the command name.
		local executable_name="$(basename "${executable_path}")"

		complete -C "${executable_name}" "${executable_name}"
	done

	# Check if the installed packages directory exists
	if ! [ -d "${DOTFILES_STATE_HOME}/pkg" ]; then
		return 0
	fi

	local installed_packages_root="${DOTFILES_STATE_HOME}/pkg"
	local package_entry=

	# Iterate through the installed packages
	find "${installed_packages_root}" -mindepth 1 -maxdepth 1 -print0 | while IFS= read -r -d '' package_entry; do

		# Nothing to do if no bin directory in the package
		if [ ! -d "${package_entry}/bin" ]; then
			continue
		fi

		# Add completion for each item
		local entry=
		for entry in ${package_entry}/bin/*; do
			local executable_name="$(basename "${entry}")"
			complete -C "$entry" "$entry"
		done
	done 
}

# Loads bash's complete function
autoload -U +X bashcompinit && bashcompinit

# no clue what this is
autoload -U compaudit zrecompile

# no clue what this is either
autoload -Uz compinit
compinit -u -d "${ZSH_COMPDUMP}"

export ZSH_CACHE_DIR="${DOTFILES_CACHE_HOME}/zsh"
export ZSH_COMPDUMP="${ZSH_CACHE_DIR}/zsh/zcompdump-${HOST}-${ZSH_VERSION}"

mkdir -p "${ZSH_CACHE_DIR}"

# zcompile the completion dump file if the .zwc is older or missing.
if command mkdir "${ZSH_COMPDUMP}.lock" 2>/dev/null; then
  zrecompile -q -p "${ZSH_COMPDUMP}"
  command rm -rf "${ZSH_COMPDUMP}.zwc.old" "${ZSH_COMPDUMP}.lock"
fi

load_completions

if have brew; then
	HOMEBREW_PREFIX="$(brew --prefix)"
	if [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]]; then
		# shellcheck disable=SC1091
		eval "$(<"${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh")"
	else
		for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*; do
			# shellcheck disable=SC1090
			[[ -r "${COMPLETION}" ]] && eval "$(<"${COMPLETION}")"
		done
	fi
fi

# fixme - the load process here seems a bit bizarre
zmodload -i zsh/complist

WORDCHARS=''

unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on successive tab press
setopt complete_in_word
setopt always_to_end

# should this be in keybindings?
bindkey -M menuselect '^o' accept-and-infer-next-history
zstyle ':completion:*:*:*:*:*' menu select

# case insensitive (all), partial-word and substring completion
if [[ "$CASE_SENSITIVE" = true ]]; then
  zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'
else
  if [[ "$HYPHEN_INSENSITIVE" = true ]]; then
    zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]-_}={[:upper:][:lower:]_-}' 'r:|=*' 'l:|=* r:|=*'
  else
    zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|=*' 'l:|=* r:|=*'
  fi
fi
unset CASE_SENSITIVE HYPHEN_INSENSITIVE

# Complete . and .. special directories
zstyle ':completion:*' special-dirs true

zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

if [[ "$OSTYPE" = solaris* ]]; then
  zstyle ':completion:*:*:*:*:processes' command "ps -u $USERNAME -o pid,user,comm"
else
  zstyle ':completion:*:*:*:*:processes' command "ps -u $USERNAME -o pid,user,comm -w -w"
fi

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path $ZSH_CACHE_DIR

# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm amanda apache at avahi avahi-autoipd beaglidx bin cacti canna \
        clamav daemon dbus distcache dnsmasq dovecot fax ftp games gdm \
        gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
        ldap lp mail mailman mailnull man messagebus mldonkey mysql nagios \
        named netdump news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
        operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
        rpc rpcuser rpm rtkit scard shutdown squid sshd statd svn sync tftp \
        usbmux uucp vcsa wwwrun xfs '_*'

# ... unless we really want to.
zstyle '*' single-ignored show

if [[ ${COMPLETION_WAITING_DOTS:-false} != false ]]; then
  expand-or-complete-with-dots() {
    # use $COMPLETION_WAITING_DOTS either as toggle or as the sequence to show
    [[ $COMPLETION_WAITING_DOTS = true ]] && COMPLETION_WAITING_DOTS="%F{red}…%f"
    # turn off line wrapping and print prompt-expanded "dot" sequence
    printf '\e[?7l%s\e[?7h' "${(%)COMPLETION_WAITING_DOTS}"
    zle expand-or-complete
    zle redisplay
  }
  zle -N expand-or-complete-with-dots
  # Set the function as the default tab completion widget
  bindkey -M emacs "^I" expand-or-complete-with-dots
  bindkey -M viins "^I" expand-or-complete-with-dots
  bindkey -M vicmd "^I" expand-or-complete-with-dots
fi

########################################################################
# Load third party applications
########################################################################

have starship && eval "$(starship init zsh)"
have ssh-agen && eval "$(ssh-agen)" &>/dev/null

########################################################################
# Keybindings
########################################################################
# 0="${ZERO:-${${0:#$ZSH_ARGZERO}:-${(%):-%N}}}"
# 0="${${(M)0:#/*}:-$PWD/$0}"
# shellcheck disable=SC1094
eval "$(<"${PACKAGE_PATH}/lib/zsh-vi-mode.zsh")"

bindkey '^k' up-history
bindkey '^j' down-history

########################################################################
# Alias
########################################################################
# Changing/making/removing directory
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

alias -- -='cd -'
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

# List directory contents
alias lsa='ls -lah'
alias l='ls -lah'
alias ll='ls -lh'
alias la='ls -lAh'

########################################################################
# Theme
########################################################################

# Make ls colorful
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
