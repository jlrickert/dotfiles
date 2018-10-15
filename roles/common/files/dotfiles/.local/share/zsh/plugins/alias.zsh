################################################################################
# Python Aliases
################################################################################
alias py='python'
alias py2='python2'

################################################################################
# Ruby Aliases
################################################################################
alias rb='ruby'

################################################################################
# External Applications
################################################################################
alias wopen='firefox_open'
alias emacs='emacs_wrapper'

################################################################################
# Pacman Aliases
################################################################################
alias pac='pacman --color auto'
alias paci='sudo pacman --color auto --sync'
alias pacI='sudo pacman --color auto --upgrades'
alias pacu='sudo pacman --color auto --sync --refresh && sudo abs'
alias pacU='sudo pacman --color auto --sync --refresh --sysupgrade'
alias pacx='sudo pacman --color auto --remove'
alias pacX='sudo pacman --color auto --remove --nosave --recursive'
alias pacs='pacman --color auto --sync --search'
alias pacq='pacman --color auto --query --info'
alias pacQ='pacman --color auto --query --search'
alias paclsorphans='sudo pacman --color auto  -Qdt'
alias pacxmorphans='sudo pacman --color auto  -Rs $(pacman -Qtdq)'

################################################################################
# Yaourt Aliases
################################################################################
alias yac='yaourt --color auto '
alias yaci='yaourt --sync'
alias yacI='yaourt --upgrades'
alias yacu='yaourt --sync --refresh && sudo abs'
alias yacU='yaourt --sync --refresh --sysupgrade --aur'
alias yacx='yaourt --remove'
alias yacX='yaourt --remove --nosave --recursive'
alias yacs='yaourt --sync --search'
alias yacq='yaourt --query --info'
alias yacQ='yaourt --query --search'
alias yaclsorphans='sudo yaourt -Qdt'
alias yacxmorphans='sudo yaourt -Rs $(pacman -Qtdq)'

################################################################################
# networking
################################################################################
alias ports='netstat -tulanp'

alias ipt='sudo /sbin/iptables'
alias iptlist='sudo /sbin/iptables -L -n -v --line-numbers'
alias iptlistin='sudo /sbin/iptables -L INPUT -n -v --line-numbers'
alias iptlistout='sudo /sbin/iptables -L OUTPUT -n -v --line-numbers'
alias iptlistfw='sudo /sbin/iptables -L FORWARD -n -v --line-numbers'
alias firewall=iptlist

################################################################################
# miscellaneous
################################################################################
alias screenshot='maim ~/Media/Screenshots/$(date +"%F-%T").png'
alias snapterm='maim -i $(xdotool getactivewindow) ~/Media/Screenshots/$(date +"%F-%T").png'
alias df="df -h"

alias meminfo='free -m -l -t -h'

###############################################################################
# get top process eating memory
###############################################################################
alias psmem='ps auxf | sort -nr -k 4'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'

# get GPU ram on desktop / laptop##
alias gpumeminfo='grep -i --color memory /var/log/Xorg.0.log'

# Grabs the disk usage in the current directory
alias usage='du -ch | grep total'

# Gets the total disk usage on your machine
alias totalusage='df -hl --total | grep total'

# Shows the individual partition usages without the temporary memory values
alias partusage='df -hlT --exclude-type=tmpfs --exclude-type=devtmpfs'

# Gives you what is using the most space. Both directories and files. Varies on
# current directory
alias most='du -hsx * | sort -rh | head -10'

alias ctime="date '+%Y-%m-%d %H:%M:%S'"
alias cdate="date '+%Y-%m-%d'"
