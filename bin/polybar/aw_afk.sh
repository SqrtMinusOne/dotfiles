#!/usr/bin/env bash
# [[file:../../Desktop.org::*aw-afk][aw-afk:1]]
afk_event=$(curl -s -X GET "http://localhost:5600/api/0/buckets/aw-watcher-afk_$(hostname)/events?limit=1" -H "accept: application/json")
status=$(echo ${afk_event} | jq -r '.[0].data.status')
afk_time=$(echo "${afk_event}" | jq -r '.[0].duration' | xargs -I !  date -u -d @! +"%H:%M")

uptime=$(datediff "$(uptime -s | xargs -I ! date -d ! -Iseconds)" "$(date -Iseconds)" -f '%H:%M' | xargs -I ! date -d ! +"%H:%M")
res="${afk_time} / ${uptime}"
if [[ $status == 'afk' ]]; then
    echo "%{u#f07178}%{+u} [AFK] $res %{u-}"
else
    echo "%{u#82aaff}%{+u} $res %{u-}"
fi
# aw-afk:1 ends here
