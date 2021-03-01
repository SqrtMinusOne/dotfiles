#!/bin/bash
hostname=$(hostname)
if [ "$hostname" = "pntk" ]; then
    TRAY_MONITOR="eDP1"
    export WLAN_INTERFACE="wlp3s0"
else
    TRAY_MONITOR="HDMI-A-0"
    export WLAN_INTERFACE="wlp35s0f3u2"
fi
declare -A FONT_SIZES=(
    ["eDP1"]="13"
    ["DVI-D-0"]="11"
    ["HDMI-A-0"]="13"
)
declare -A EMOJI_SCALE=(
    ["eDP1"]="9"
    ["DVI-D-0"]="10"
    ["HDMI-A-0"]="10"
)
declare -A BAR_HEIGHT=(
    ["eDP1"]="29"
    ["DVI-D-0"]="23"
    ["HDMI-A-0"]="29"
)
declare -A BLOCKS=(
    ["DVI-D-0"]="pulseaudio SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP weather SEP sun aw-afk date TSEP"
    ["HDMI-A-0"]="pulseaudio mpd SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP weather SEP sun aw-afk date TSEP"
    ["eDP1"]="pulseaudio mpd SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP battery SEP sun aw-afk date TSEP"
)

export LOC="SPB"

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
    if [[ -z "$SCALE" ]]; then
        continue
    fi
    export FONT0="pango:monospace:size=$SIZE;1"
    export FONT1="NotoEmoji:scale=$SCALE:antialias=false;1"
    export FONT2="fontawesome:pixelsize=$SIZE;1"
    export FONT3="JetBrains Mono Nerd Font:monospace:size=$SIZE;1"
    export HEIGHT=${BAR_HEIGHT[$MONITOR]}
    export RIGHT_BLOCKS=${BLOCKS[$MONITOR]}
    polybar mybar &
done
