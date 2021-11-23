#!/usr/bin/env bash
# [[file:../../Desktop.org::*openvpn][openvpn:1]]
vpn=$(pgrep -a openvpn$ | head -n 1 | awk '{print $NF }' | cut -d '.' -f 1)
if [ -n "$vpn" ]; then
    echo "  "
else
    echo "  "
fi
# openvpn:1 ends here
