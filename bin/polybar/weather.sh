#!/usr/bin/env bash
# [[file:../../Desktop.org::*weather][weather:1]]
# Configuration - can be overridden by environment variables
bar_format="${BAR_FORMAT:-"%i %t"}"  # %t = temp, %c = condition, %i = icon, %w = wind
location="${LOCATION:-"St+Petersburg"}"
units="${UNITS:-celsius}"  # celsius or fahrenheit

# WMO weather code to emoji mapping
get_weather_icon() {
    case $1 in
        0) echo "☀️";;           # Clear sky
        1) echo "🌤️";;           # Mainly clear
        2) echo "⛅";;           # Partly cloudy
        3) echo "☁️";;           # Overcast
        45|48) echo "🌫️";;       # Fog
        51|53|55) echo "🌧️";;    # Drizzle
        56|57) echo "🌧️";;       # Freezing drizzle
        61|63|65) echo "🌧️";;    # Rain
        66|67) echo "🌨️";;       # Freezing rain
        71|73|75|77) echo "❄️";; # Snow
        80|81|82) echo "🌧️";;    # Showers
        85|86) echo "🌨️";;       # Snow showers
        95) echo "⛈️";;          # Thunderstorm
        96|99) echo "⛈️";;       # Thunderstorm with hail
        *) echo "?";;
    esac
}

# WMO weather code to description
get_weather_desc() {
    case $1 in
        0) echo "Clear";;
        1) echo "Mostly Clear";;
        2) echo "Partly Cloudy";;
        3) echo "Cloudy";;
        45) echo "Fog";;
        48) echo "Rime Fog";;
        51) echo "Light Drizzle";;
        53) echo "Drizzle";;
        55) echo "Heavy Drizzle";;
        56|57) echo "Freezing Drizzle";;
        61) echo "Light Rain";;
        63) echo "Rain";;
        65) echo "Heavy Rain";;
        66|67) echo "Freezing Rain";;
        71) echo "Light Snow";;
        73) echo "Snow";;
        75) echo "Heavy Snow";;
        77) echo "Snow Grains";;
        80) echo "Light Showers";;
        81) echo "Showers";;
        82) echo "Heavy Showers";;
        85|86) echo "Snow Showers";;
        95) echo "Thunderstorm";;
        96|99) echo "Thunderstorm+Hail";;
        *) echo "Unknown";;
    esac
}

# Get coordinates from city name using Open-Meteo Geocoding API
geo_response=$(curl -sf "https://geocoding-api.open-meteo.com/v1/search?name=${location}&count=1" 2>/dev/null)
if [ -z "$geo_response" ]; then
    echo "??"
    exit 1
fi

lat=$(echo "$geo_response" | jq -r '.results[0].latitude // empty')
lon=$(echo "$geo_response" | jq -r '.results[0].longitude // empty')

if [ -z "$lat" ] || [ -z "$lon" ]; then
    echo "??"
    exit 1
fi

# Build temperature unit parameter
temp_unit="celsius"
[ "$units" = "fahrenheit" ] && temp_unit="fahrenheit"

# Get current weather from Open-Meteo API
weather_response=$(curl -sf "https://api.open-meteo.com/v1/forecast?latitude=${lat}&longitude=${lon}&current=temperature_2m,weather_code,wind_speed_10m&temperature_unit=${temp_unit}" 2>/dev/null)
if [ -z "$weather_response" ]; then
    echo "??"
    exit 1
fi

# Parse weather data
temp=$(echo "$weather_response" | jq -r '.current.temperature_2m // empty')
code=$(echo "$weather_response" | jq -r '.current.weather_code // empty')
wind=$(echo "$weather_response" | jq -r '.current.wind_speed_10m // empty')

if [ -z "$temp" ] || [ -z "$code" ]; then
    echo "??"
    exit 1
fi

# Get icon and description
icon=$(get_weather_icon "$code")
desc=$(get_weather_desc "$code")

# Round temperature
temp_rounded=$(printf "%.0f" "$temp")

# Unit symbol
unit_symbol="°C"
[ "$units" = "fahrenheit" ] && unit_symbol="°F"

# Format output based on BAR_FORMAT
output="$bar_format"
output="${output//%t/${temp_rounded}${unit_symbol}}"
output="${output//%c/${desc}}"
output="${output//%i/${icon}}"
output="${output//%w/${wind}km\/h}"

echo "$output"
# weather:1 ends here
