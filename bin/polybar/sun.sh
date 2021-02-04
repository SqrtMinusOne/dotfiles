#1/bin/bash
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

function send_report {
    report="$(sunwait report ${LAT} ${LON} | sed 's/^[[:space:]]*//gm')"
    notify-send "Sunwait report" "$report"
}

case $BLOCK_BUTTON in
    1) send_report
esac

if [[ ${time} == 'DAY' ]]; then
    sunset=$(sunwait list daylight set ${LAT} ${LON})
    echo "%{u#ffcb6b}%{+u} $sunset %{u-}"
else
    sunrise=$(sunwait list daylight rise ${LAT} ${LON})
    echo "%{u#f07178}%{+u} $sunrise %{u-}"
fi
