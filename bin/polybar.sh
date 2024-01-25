#!/usr/bin/env bash
# [[file:../Desktop.org::*Launch script][Launch script:1]]
hostname=$(hostname)
# Settings varying on the hostname
if [ "$hostname" = "azure" ]; then
    TRAY_MONITOR="eDP-1"
elif [ "$hostname" = "eminence" ]; then
    if xrandr --query | grep " connected" | cut -d" " -f1 | grep -q "HDMI-A-0"; then
        TRAY_MONITOR="HDMI-A-0"
    else
        TRAY_MONITOR="eDP"
    fi
elif [ "$hostname" = "iris" ]; then
    TRAY_MONITOR="HDMI-1"
else
    TRAY_MONITOR="DP-1"
fi

# Setting varying on the monitor
declare -A FONT_SIZES=(
    ["eDP"]="13"
    ["eDP-1"]="13"
    ["DVI-D-0"]="13"
    ["HDMI-A-0"]="13"
    ["HDMI-1"]="13"
    ["HDMI-0"]="13"
    ["DP-1"]="13"
)
declare -A EMOJI_SCALE=(
    ["eDP"]="9"
    ["eDP-1"]="9"
    ["DVI-D-0"]="10"
    ["HDMI-A-0"]="10"
    ["HDMI-1"]="10"
    ["HDMI-0"]="10"
    ["DP-1"]="10"
)
declare -A BAR_HEIGHT=(
    ["eDP"]="29"
    ["eDP-1"]="29"
    ["DVI-D-0"]="29"
    ["HDMI-A-0"]="29"
    ["HDMI-1"]="29"
    ["HDMI-0"]="29"
    ["DP-1"]="29"
)
declare -A BLOCKS=(
    ["eDP"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--cyan cpu glyph-cyan--cyan temperature glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["eDP-1"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--cyan cpu glyph-cyan--cyan temperature glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["DVI-D-0"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--cyan cpu glyph-cyan--cyan temperature glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["HDMI-A-0"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--cyan cpu glyph-cyan--cyan temperature glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["HDMI-1"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--cyan cpu glyph-cyan--cyan temperature glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["HDMI-0"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--light-cyan nvidia glyph-light-cyan--cyan cpu glyph-cyan--cyan temperature glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["DP-1"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--light-cyan nvidia glyph-light-cyan--cyan cpu glyph-cyan--cyan temperature glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
)

# Geolocation for some modules
export LOC="SPB"

# export IPSTACK_API_KEY=$(pass show My_Online/APIs/ipstack | head -n 1)

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
    # export FONT0="pango:monospace:size=$SIZE;1"
    # export FONT1="NotoEmoji:scale=$SCALE:antialias=false;1"
    # export FONT2="fontawesome:pixelsize=$SIZE;1"
    # export FONT3="JetBrains Mono Nerd Font:monospace:size=15;1"
    export HEIGHT=${BAR_HEIGHT[$MONITOR]}
    export RIGHT_BLOCKS=${BLOCKS[$MONITOR]}
    polybar mybar &
done
# Launch script:1 ends here
