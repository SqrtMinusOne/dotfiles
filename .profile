# [[file:Shell.org::*Environment][Environment:1]]
export EDITOR=/usr/bin/vim
export BROWSER=/usr/bin/firefox
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
# Environment:1 ends here

# [[file:Shell.org::*Various paths][Various paths:1]]
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
    export PATH="$HOME/bin/scripts:$PATH"
fi
# Various paths:1 ends here

# [[file:Shell.org::*Various paths][Various paths:2]]
if [ -d "/usr/local/texlive/2020" ]; then
    export MANPATH="/usr/local/texlive/2020/texmf-dist/doc/man:$MANPATH"
    export INFOPATH="/usr/local/texlive/2020/texmf-dist/doc/info:$INFOPATH"
    export PATH="/usr/local/texlive/2020/bin/x86_64-linux:$PATH"
fi
# Various paths:2 ends here

# [[file:Shell.org::*Various paths][Various paths:3]]
if [ -d "$HOME/.cargo" ] ; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi
# Various paths:3 ends here

# [[file:Shell.org::*Various paths][Various paths:4]]
if [ -d "$HOME/.rvm" ] ; then
    export PATH="$PATH:$HOME/.rvm/bin"
fi
# if [ -d "$HOME/.gem" ]; then
#     export PATH="$HOME/.gem/ruby/2.7.0/bin:$PATH"
# fi
# Various paths:4 ends here

# [[file:Shell.org::*Various paths][Various paths:5]]
if [ -d "$HOME/go" ] ; then
    export PATH="$HOME/go/bin:$PATH"
fi
# Various paths:5 ends here

# [[file:Shell.org::*Various paths][Various paths:6]]
[ -f "/home/pavel/.ghcup/env" ] && source "/home/pavel/.ghcup/env" # ghcup-env
# Various paths:6 ends here

# [[file:Shell.org::*Various paths][Various paths:7]]
if [ -d "$HOME/perl5" ] ; then
    PATH="/home/pavel/perl5/bin${PATH:+:${PATH}}"
    PERL5LIB="/home/pavel/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
    PERL_LOCAL_LIB_ROOT="/home/pavel/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
    PERL_MB_OPT="--install_base \"/home/pavel/perl5\""; export PERL_MB_OPT;
    PERL_MM_OPT="INSTALL_BASE=/home/pavel/perl5"; export PERL_MM_OPT;
fi
# Various paths:7 ends here
