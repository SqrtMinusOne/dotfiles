#!/usr/bin/env bash
# [[file:../../Desktop.org::*C-g][C-g:1]]
EMACS_FLAG="-l /home/pavel/.emacs.d/desktop.el"
EXCLUDE_PATTERN="dbus-launch --exit-with-session emacs"
EMACS_PIDS=$(pgrep -f "emacs.*${EMACS_FLAG}")
SIGNAL_SENT=false

for PID in $EMACS_PIDS; d   o
    CMDLINE=$(ps -p "$PID" -o args=)

    if [[ "$CMDLINE" == *"$EXCLUDE_PATTERN"* ]]; then
        continue
    fi

    kill -SIGUSR2 "$PID" 2>/dev/null

    if [ $? -eq 0 ]; then
        echo "Sent SIGUSR2 to Emacs (PID: $PID)"
        SIGNAL_SENT=true
    else
        echo "Failed to send SIGUSR2 to Emacs (PID: $PID)"
    fi
done

if [ "$SIGNAL_SENT" = false ]; then
    echo "Emacs process not found or already handled."
    exit 1
fi

exit 0
# C-g:1 ends here
