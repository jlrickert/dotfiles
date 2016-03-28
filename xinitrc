#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Map keys with xmodmap
xmodmap ~/.Xmodmap

feh --randomize --bg-fill $HOME/Media/backgrounds/*

# NetworkManager for managing networks.
nm-applet &

# Hide the cursor when idle.
unclutter &

# Notifications with dunst
dunst &

# Numlock  on by default
numlockx &

exec i3
