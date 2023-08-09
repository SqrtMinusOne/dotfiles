# [[file:Console.org::*Startup & environment][Startup & environment:1]]
export SHELL
# Startup & environment:1 ends here

# [[file:Console.org::*Startup & environment][Startup & environment:2]]
if [[ $- != *i* ]]
then
    [[ -n "$SSH_CLIENT" && -f "/etc/bashrc" ]] && source /etc/profile
    return
fi
# Startup & environment:2 ends here

# [[file:Console.org::*Startup & environment][Startup & environment:3]]
if command -v termux-setup-storage > /dev/null; then
    if [[ -z "$IS_ANDROID" ]]; then
        source ~/.profile
    fi
fi
# Startup & environment:3 ends here

# [[file:Console.org::*Startup & environment][Startup & environment:4]]
if [[ -f "/etc/bashrc" ]]; then
    source /etc/bashrc
fi
# Startup & environment:4 ends here

# [[file:Console.org::*Startup & environment][Startup & environment:5]]
xhost +local:root > /dev/null 2>&1
# Startup & environment:5 ends here

# [[file:Console.org::*Startup & environment][Startup & environment:6]]
export MANPAGER="sh -c 'sed -e s/.\\\\x08//g | bat -l man -p'"
# Startup & environment:6 ends here

# [[file:Console.org::*Startup & environment][Startup & environment:7]]
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/bash"
# Startup & environment:7 ends here

# [[file:Console.org::*Launch fish][Launch fish:1]]
use_fish=true

if [[ $(ps --no-header --pid=$PPID --format=cmd) != "fish" && ${use_fish} && $(command -v fish) ]]
then
    exec fish
fi
# Launch fish:1 ends here

# [[file:Console.org::*Colors][Colors:1]]
use_color=true

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.  Use internal bash
# globbing instead of external grep binary.
safe_term=${TERM//[^[:alnum:]]/?}   # sanitize TERM
match_lhs=""
[[ -f ~/.dir_colors   ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs}    ]] \
    && type -P dircolors >/dev/null \
    && match_lhs=$(dircolors --print-database)
[[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] && use_color=true

if ${use_color} ; then
    # Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
    if type -P dircolors >/dev/null ; then
        if [[ -f ~/.dir_colors ]] ; then
            eval $(dircolors -b ~/.dir_colors)
        elif [[ -f /etc/DIR_COLORS ]] ; then
            eval $(dircolors -b /etc/DIR_COLORS)
        fi
    fi

    if [[ ${EUID} == 0 ]] ; then
        PS1='\[\033[01;31m\][\h\[\033[01;36m\] \W\[\033[01;31m\]]\$\[\033[00m\] '
    else
        PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\]]\$\[\033[00m\] '
    fi

    alias ls='ls --color=auto'
    alias grep='grep --colour=auto'
    alias egrep='egrep --colour=auto'
    alias fgrep='fgrep --colour=auto'
else
    if [[ ${EUID} == 0 ]] ; then
        # show root@ when we don't have colors
        PS1='\u@\h \W \$ '
    else
        PS1='\u@\h \w \$ '
    fi
fi

unset use_color safe_term match_lhs sh
# Colors:1 ends here

# [[file:Console.org::*Settings][Settings:1]]
complete -cf sudo           # Sudo autocompletion

shopt -s checkwinsize       # Check windows size after each command
shopt -s expand_aliases     # Aliases
shopt -s autocd             # Cd to directory just by typing its name (without cd)
# Settings:1 ends here

# [[file:Console.org::*Settings][Settings:2]]
shopt -s histappend
export HISTCONTROL=ignoredups:erasedups
HISTSIZE=
HISTFILESIZE=
# Settings:2 ends here

# [[file:Console.org::*Aliases][Aliases:1]]
alias v="vim"
if command -v exa > /dev/null; then
    alias ls="exa --icons"
    alias ll="exa -lah --icons"
else
    alias ll='ls -lah'
fi
alias q="exit"
alias c="clear"
alias ci="init_mamba"
alias ca="micromamba activate"
alias cii="export INIT_MAMBA=true && init_mamba"
# Aliases:1 ends here

# [[file:Console.org::*Aliases][Aliases:2]]
if [[ ! -z "$SIMPLE" ]]; then
    unalias ls
    alias ll="ls -lah"
fi
# Aliases:2 ends here

# [[file:Console.org::*Micromamba][Micromamba:1]]
init_mamba () {
    export MAMBA_EXE="/gnu/store/w0rrglxs2247nr4wawrh5dylisjra1q4-micromamba-bin-1.4.4-0/bin/micromamba";
    export MAMBA_ROOT_PREFIX="/home/pavel/micromamba";
    __mamba_setup="$("$MAMBA_EXE" shell hook --shell bash --prefix "$MAMBA_ROOT_PREFIX" 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__mamba_setup"
    else
        if [ -f "/home/pavel/micromamba/etc/profile.d/micromamba.sh" ]; then
            . "/home/pavel/micromamba/etc/profile.d/micromamba.sh"
        else
            export  PATH="/home/pavel/micromamba/bin:$PATH"  # extra space after export prevents interference from conda init
        fi
    fi
    unset __mamba_setup
}

if [[ ! -z "$INIT_MAMBA" ]]; then
    init_mamba
fi
# Micromamba:1 ends here

# [[file:Console.org::*Starship][Starship:1]]
if [[ -z "$SIMPLE" && "$TERM" != "dumb" ]]; then
    eval "$(starship init bash)"
fi
# Starship:1 ends here

# [[file:Console.org::*Yandex Cloud][Yandex Cloud:1]]
init_yc () {
    # The next line updates PATH for Yandex Cloud CLI.
    if [ -f '/home/pavel/yandex-cloud/path.bash.inc' ]; then source '/home/pavel/yandex-cloud/path.bash.inc'; fi

    # The next line enables shell command completion for yc.
    if [ -f '/home/pavel/yandex-cloud/completion.bash.inc' ]; then source '/home/pavel/yandex-cloud/completion.bash.inc'; fi
}
# Yandex Cloud:1 ends here
