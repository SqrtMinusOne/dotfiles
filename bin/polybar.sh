#!/bin/bash
# TRAY_MONITOR="HDMI-A-0"
TRAY_MONITOR="DVI-D-0"
declare -A FONT_SIZES=(
    ["DVI-D-0"]="11"
    ["HDMI-A-0"]="13"
)
declare -A EMOJI_SCALE=(
    ["DVI-D-0"]="10"
    ["HDMI-A-0"]="10"
)
declare -A BAR_HEIGHT=(
    ["DVI-D-0"]="23"
    ["HDMI-A-0"]="29"
)
declare -A BLOCKS=(
    ["DVI-D-0"]="pulseaudio SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP sun aw-afk date TSEP"
    ["HDMI-A-0"]="pulseaudio mpd SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP sun aw-afk date TSEP"
)

pkill polybar
for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    export MONITOR=$m
    if [ "$MONITOR" = "$TRAY_MONITOR" ]; then
        export TRAY="right"
    else
        export TRAY="none"
    fi
    SIZE=${FONT_SIZES[$MONITOR]}
    SCALE=${EMOJI_SCALE[$MONITOR]}
    export FONT0="pango:monospace:size=$SIZE;1"
    export FONT1="NotoEmoji:scale=$SCALE:antialias=false;1"
    export FONT2="fontawesome:pixelsize=$SIZE;1"
    export FONT3="JetBrains Mono Nerd Font:monospace:size=$SIZE;1"
    export HEIGHT=${BAR_HEIGHT[$MONITOR]}
    export RIGHT_BLOCKS=${BLOCKS[$MONITOR]}
    polybar --reload mybar &
done
