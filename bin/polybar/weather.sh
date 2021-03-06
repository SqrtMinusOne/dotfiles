#!/usr/bin/env bash
# [[file:../../Desktop.org::*weather][weather:1]]
bar_format="${BAR_FORMAT:-"%t"}"
location="${LOCATION:-"Saint-Petersburg"}"
format_1=${FORMAT_1:-"qF"}
format_2=${FORMAT_1:-"format=v2n"}

bar_weather=$(curl -s wttr.in/${location}?format=${bar_format} || echo "??")
if [ -z "$bar_weather" ]; then
    exit 1
elif [[ "$bar_weather" == *"Unknown"* || "$bar_weather" == *"Sorry"* || "$bar_weather" == *"Bad Gateway"* ]]; then
    echo "??"
    exit 1
else
    echo ${bar_weather}
fi
# weather:1 ends here
