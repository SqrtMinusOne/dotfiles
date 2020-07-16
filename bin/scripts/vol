#!/bin/bash
SINK=`pactl list short | grep RUNNING | sed -e 's,^\([0-9][0-9]*\)[^0-9].*,\1,'`
if [[ $SINK ]]; then
    if [[ $1 == 'volume' ]]; then
        pactl set-sink-volume ${SINK} $2
    fi
    if [[ $1 == 'mute' ]]; then
        pactl set-sink-mute ${SINK} toggle
    fi
fi
