hostname=$(hostname)
if [ "$hostname" = "pdsk" ] ; then
    bash "$HOME/bin/bukuserver.sh" &
    bash "$HOME/bin/tabliss.sh" &
fi
