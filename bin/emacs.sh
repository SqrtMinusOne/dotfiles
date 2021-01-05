if [ -d "$HOME/.cargo" ] ; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi
if [ -d "$HOME/.rvm" ] ; then
    export PATH="$PATH:$HOME/.rvm/bin"
fi
if [ -d "$HOME/go" ] ; then
    export PATH="$HOME/go/bin:$PATH"
fi

[ -f "/home/pavel/.ghcup/env" ] && source "/home/pavel/.ghcup/env" # ghcup-env
emacs
