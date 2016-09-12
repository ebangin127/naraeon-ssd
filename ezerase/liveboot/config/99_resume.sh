#!/bin/sh

case "$1" in
    resume)
        EXTERNAL=$(xrandr | grep -E " connected" | sed -e "s/\([A-Z0-9]\+\) connected.*/\1/")
        xrandr --output $EXTERNAL --off
        xrandr --output $EXTERNAL --auto
esac