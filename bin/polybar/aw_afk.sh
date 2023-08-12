#!/usr/bin/env bash
# [[file:../../Desktop.org::*aw-afk][aw-afk:1]]
afk_event=$(curl -s -X GET "http://localhost:5600/api/0/buckets/aw-watcher-afk_$(hostname)/events?limit=1" -H "accept: application/json")
status=$(echo ${afk_event} | jq -r '.[0].data.status')
afk_time=$(echo "${afk_event}" | jq -r '.[0].duration' | xargs -I !  date -u -d @! +"%H:%M")

uptime=$(uptime | awk '{ print substr($3, 0, length($3) - 1) }' | xargs -I ! date -d ! +"%H:%M")
res="${afk_time} / ${uptime}"
if [[ $status == 'afk' ]]; then
    # echo "%{u#cc3333}%{+u} [AFK] $res %{u-}"
    echo "[AFK] $res"
else
    # echo "%{u#375cd8}%{+u} $res %{u-}"
    echo "$res"
fi
# aw-afk:1 ends here
