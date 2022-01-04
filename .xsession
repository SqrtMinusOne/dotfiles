#!/usr/bin/env bash
# [[file:Desktop.org::*Xsession][Xsession:1]]
# Source .profile
. ~/.profile

# Disable access control for the current user
xhost +SI:localuser:$USER

# Fix for Java applications
export _JAVA_AWT_WM_NONREPARENTING=1

# Apply XResourses
xrdb -merge ~/.Xresources

# Turn off the system bell
xset -b

# Use i3lock as a screen locker
xss-lock -- i3lock -f -i /home/pavel/Pictures/lock-wallpaper.png &

# Some apps that have to be launched only once.
picom &
# nm-applet &
dunst &
copyq &

# Run the Emacs startup script as a session.
# exec dbus-launch --exit-with-session ~/.emacs.d/run-exwm.sh
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/desktop.el
# Xsession:1 ends here
