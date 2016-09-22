#!/usr/bin/env sh
# TODO: rewrite this as a series of bash scripts that the various
# window managers I have installed execute upon startup so that they
# can each have different options.

# load Xresources
[[ -f ~/.Xresources ]] && xrdb ~/.Xresources

# notifications daemon
dunst &

# load wallpapers
nitrogen --restore 
