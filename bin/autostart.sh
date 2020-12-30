hostname=$(hostname)
if [ "$hostname" = "pdsk" ] ; then
    bash "$HOME/bin/autostart.sh" &
    bash "$HOME/bin/tabliss.sh" &
fi
