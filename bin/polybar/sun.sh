#!/usr/bin/env bash
# [[file:../../Desktop.org::*sun][sun:1]]
declare -A LAT_DATA=(
    ["TMN"]="57.15N"
    ["SPB"]="59.9375N"
)
declare -A LON_DATA=(
    ["TMN"]="65.533333E"
    ["SPB"]="30.308611E"
)
if [ -z "$LOC" ]; then
    echo "LOC?"
    exit -1
fi
LAT=${LAT_DATA[$LOC]}
LON=${LON_DATA[$LOC]}

time=$(sunwait poll daylight rise ${LAT} $LON)

if [[ ${time} == 'DAY' ]]; then
    sunset=$(sunwait list daylight set ${LAT} ${LON})
    # echo "%{u#8a5d00}%{+u} $sunset %{u-}"
    echo $sunset
else
    sunrise=$(sunwait list daylight rise ${LAT} ${LON})
    # echo "%{u#cc3333}%{+u} $sunrise %{u-}"
    echo $sunrise
fi
# sun:1 ends here
