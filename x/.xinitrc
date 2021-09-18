#!/bin/sh

xsetroot -cursor_name left_ptr
xdotool mousemove 1 1920
setxkbmap -option 'caps:ctrl_modifier'
xcape -e 'Caps_Lock=Escape'

[ -f ~/.xprofile ] && . ~/.xprofile

#exec dwm

#export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/1000/bus"

#while type dwm >/dev/null ; do dwm && continue || break ; done

exec xmonad

