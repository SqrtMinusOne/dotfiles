#1/bin/bash
LAT=59.9375N
LON=30.308611E

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
