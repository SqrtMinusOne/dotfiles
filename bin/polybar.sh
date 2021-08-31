#!/usr/bin/env bash
# [[file:../Desktop.org::*Launching][Launching:1]]
hostname=$(hostname)
# Settings varying on the hostname
export WLAN_INTERFACE=$(nmcli -f DEVICE con show | grep -Ev "(.*docker.*|DEVICE|br.*|tun.*|veth.*|--)" | xargs)
if [ "$hostname" = "azure" ]; then
    TRAY_MONITOR="eDP-1"
    # export WLAN_INTERFACE="wlp3s0"
elif [ "$hostname" = "eminence" ]; then
    TRAY_MONITOR="eDP"
    # export WLAN_INTERFACE="wlo1"
else
    TRAY_MONITOR="HDMI-A-0"
    # export WLAN_INTERFACE="wlp35s0f3u2"
fi

# Setting varying on the monitor
declare -A FONT_SIZES=(
    ["eDP"]="13"
    ["eDP-1"]="13"
    ["DVI-D-0"]="13"
    ["HDMI-A-0"]="13"
)
declare -A EMOJI_SCALE=(
    ["eDP"]="9"
    ["eDP-1"]="9"
    ["DVI-D-0"]="10"
    ["HDMI-A-0"]="10"
)
declare -A BAR_HEIGHT=(
    ["eDP"]="29"
    ["eDP-1"]="29"
    ["DVI-D-0"]="29"
    ["HDMI-A-0"]="29"
)
declare -A BLOCKS=(
    ["eDP"]="pulseaudio mpd SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP battery SEP sun aw-afk date TSEP"
    ["eDP-1"]="pulseaudio mpd SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP battery SEP sun aw-afk date TSEP"
    ["DVI-D-0"]="pulseaudio mpd SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP weather SEP sun aw-afk date TSEP"
    ["HDMI-A-0"]="pulseaudio mpd SEP cpu ram-memory swap-memory SEP network ipstack-vpn SEP xkeyboard SEP weather SEP sun aw-afk date TSEP"
)

# Geolocation for some modules
export LOC="SPB"

export IPSTACK_API_KEY=$(pass show My_Online/APIs/ipstack | head -n 1)

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
# Launching:1 ends here
