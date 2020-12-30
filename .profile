export EDITOR=/usr/bin/nvim
export BROWSER=/usr/bin/qutebrowser
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

export MANPATH="/usr/local/texlive/2020/texmf-dist/doc/man:$MANPATH"
export INFOPATH="/usr/local/texlive/2020/texmf-dist/doc/info:$INFOPATH"
export PATH="/usr/local/texlive/2020/bin/x86_64-linux:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
    export PATH="$HOME/bin/scripts:$PATH"
fi

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
