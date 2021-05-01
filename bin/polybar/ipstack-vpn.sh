#!/bin/bash
# [[file:../../Desktop.org::*ipstack-vpn][ipstack-vpn:1]]
ip=$(dig +short +timeout=1 myip.opendns.com @resolver1.opendns.com 2> /dev/null)
API_KEY="$(cat ~/secrets/ipstack-api-key)"
if [[ -z $ip || $ip == *"timed out"* ]]; then
    echo "%{u#f07178}%{+u} ?? %{u-}"
    exit
fi
ip_info=$(curl -s http://api.ipstack.com/${ip}?access_key=${API_KEY})
# emoji=$(echo $ip_info | jq -r '.location.country_flag_emoji')
code=$(echo $ip_info | jq -r '.country_code')
vpn=$(pgrep -a openvpn$ | head -n 1 | awk '{print $NF }' | cut -d '.' -f 1)

if [ -n "$vpn" ]; then
    echo "%{u#82aaff}%{+u}  $code %{u-}"
else
    echo "%{u#f07178}%{+u}  $code %{u-}"
fi
# ipstack-vpn:1 ends here
