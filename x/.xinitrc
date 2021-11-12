#!/bin/sh

#xsetroot -cursor_name left_ptr
#xdotool mousemove 1 1920
#setxkbmap -option 'caps:ctrl_modifier'
#xcape -e 'Caps_Lock=Escape'

[ -f ~/.xprofile ] && . ~/.xprofile

#while type dwm >/dev/null ; do dwm && continue || break ; done
#exec xmonad
# exec dwm
exec cinnamon-session
