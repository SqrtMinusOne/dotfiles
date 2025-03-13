#!/usr/bin/env bash
# [[file:../Desktop.org::*Launch script][Launch script:1]]
hostname=$(hostname)
# Settings varying on the hostname
DPI=96
BAR_HEIGHT=29

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
elif [ "$hostname" = "amaranth" ]; then
    export BATTERY="axp20x-battery"
    export ADAPTER="axp22x-ac"
    TRAY_MONITOR="DSI-1"
else
    TRAY_MONITOR="DP-1"
fi

# Setting varying on the monitor
declare -A FONT_SIZE_OVERRIDE=(
    ["amaranth:DSI-1"]="13"
)
declare -A EMOJI_SCALE_OVERRIDE=(
    ["amaranth:DSI-1"]="13"
)
declare -A DPI_OVERRIDE=(
    ["amaranth:DSI-1"]="140"
)
declare -A BAR_HEIGHT_OVERRIDE=(
    ["amaranth:DSI-1"]="40"
)
declare -A BLOCKS=(
    ["violet:HDMI-0"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--light-cyan nvidia glyph-light-cyan--light-cyan backlight glyph-light-cyan--cyan cpu glyph-cyan--cyan temperature glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["violet:DP-1"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--light-cyan nvidia glyph-light-cyan--light-cyan backlight glyph-light-cyan--cyan cpu glyph-cyan--cyan temperature glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["eminence:eDP"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--light-cyan nvidia glyph-light-cyan--light-cyan backlight glyph-light-cyan--cyan cpu glyph-cyan--cyan temperature glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["eminence:HDMI-A-0"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--light-cyan nvidia glyph-light-cyan--light-cyan backlight glyph-light-cyan--cyan cpu glyph-cyan--cyan temperature glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["iris:HDMI-1"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--magenta mpd glyph-magenta--light-cyan nvidia glyph-light-cyan--light-cyan backlight glyph-light-cyan--cyan cpu glyph-cyan--cyan temperature glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--green swap-memory glyph-green--light-red bandwidth openvpn glyph-light-red--red xkeyboard glyph-red--light-yellow weather glyph-light-yellow--yellow sun glyph-yellow--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
    ["amaranth:DSI-1"]="glyph-background--light-magenta pulseaudio glyph-light-magenta--light-cyan backlight glyph-light-cyan--cyan cpu glyph-cyan--cyan battery glyph-cyan--light-green ram-memory glyph-light-green--red xkeyboard glyph-red--light-blue aw-afk glyph-light-blue--blue date glyph-blue--background "
)

declare -A TEMP_HWMON_PATHS=(
    ["eminence"]="/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon2/temp1_input"
    ["indigo"]="/sys/devices/platform/coretemp.0/hwmon/hwmon2/temp1_input"
    ["violet"]="/sys/devices/platform/coretemp.0/hwmon/hwmon2/temp1_input"
    ["amaranth"]="/sys/devices/virtual/thermal/thermal_zone0/hwmon0/temp1_input"
)

declare -A BACKLIGHT_CARDS=(
    ["amaranth"]="backlight@0"
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

    KEY="$(hostname):${m}"

    export HEIGHT=${BAR_HEIGHT_OVERRIDE[$KEY]:-$BAR_HEIGHT}
    export DPI=${DPI_OVERRIDE[$KEY]:-$DPI}
    export RIGHT_BLOCKS=${BLOCKS[$KEY]}
    if [[ -z "$RIGHT_BLOCKS" ]]; then
        continue
    fi

    export TEMP_HWMON_PATH=${TEMP_HWMON_PATHS[$(hostname)]}
    export BACKLIGHT_CARD=${BACKLIGHT_CARDS[$(hostname)]}

    polybar mybar &
done
# Launch script:1 ends here
