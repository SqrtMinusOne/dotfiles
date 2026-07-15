#!/usr/bin/env bash
# [[file:../Desktop.org::*Launch script][Launch script:1]]
hostname=$(hostname)

TRAY_MONITOR=$(xrandr --query | grep " connected" | grep -oE "^[^ ]+ connected (primary )?[0-9]" | head -n 1 | cut -d" " -f1)

if [ "$hostname" = 'violet' ]; then
    TRAY_MONITOR="DP-1"
fi

# Settings varying by hostname
declare -A BLOCKS=(
    ["azure"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--cyan cpu glyph-cyan--cyan temperature glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["eminence"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--cyan cpu glyph-cyan--cyan temperature glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["iris"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--cyan cpu glyph-cyan--cyan temperature glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["indigo"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--cyan cpu glyph-cyan--cyan temperature glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["violet"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--light-cyan nvidia glyph-light-cyan--cyan cpu glyph-cyan--cyan temperature glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["weiss"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--cyan cpu glyph-cyan--cyan temperature glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["default"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--cyan cpu glyph-cyan--cyan temperature glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
)

declare -A TEMP_HWMON_PATHS=(
    ["eminence"]="/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon2/temp1_input"
    ["indigo"]="/sys/devices/platform/coretemp.0/hwmon/hwmon2/temp1_input"
    ["violet"]="/sys/devices/platform/coretemp.0/hwmon/hwmon2/temp1_input"
)

# Geolocation for some modules
export LOC="SPB"

# export IPSTACK_API_KEY=$(pass show My_Online/APIs/ipstack | head -n 1)

pkill polybar
RIGHT_BLOCKS=${BLOCKS[$hostname]:-${BLOCKS[default]}}
for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    export MONITOR=$m
    if [ "$MONITOR" = "$TRAY_MONITOR" ]; then
        export TRAY="right"
    else
        export TRAY="none"
    fi
    export HEIGHT=29
    export RIGHT_BLOCKS
    export TEMP_HWMON_PATH=${TEMP_HWMON_PATHS[$hostname]}
    polybar mybar &
done
# Launch script:1 ends here
