#!/bin/sh
# Start slstatus
slstatus &

# Restore wallpaper with nitrogen
nitrogen --restore &

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Start DWM
exec dwm
