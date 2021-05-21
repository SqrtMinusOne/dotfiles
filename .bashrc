# [[file:Console.org::*Startup & environment][Startup & environment:1]]
export SHELL
# Startup & environment:1 ends here

# [[file:Console.org::*Startup & environment][Startup & environment:2]]
if [[ $- != *i* ]]
then
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile
    return
fi
# Startup & environment:2 ends here

# [[file:Console.org::*Startup & environment][Startup & environment:3]]
source /etc/bashrc
# Startup & environment:3 ends here

# [[file:Console.org::*Startup & environment][Startup & environment:4]]
xhost +local:root > /dev/null 2>&1
# Startup & environment:4 ends here

# [[file:Console.org::*Startup & environment][Startup & environment:5]]
export MANPAGER="sh -c 'sed -e s/.\\\\x08//g | bat -l man -p'"
# Startup & environment:5 ends here

# [[file:Console.org::*Launch fish][Launch fish:1]]
use_fish=true

if [[ $(ps --no-header --pid=$PPID --format=cmd) != "fish" && ${use_fish} ]]
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
alias gg="lazygit"
alias ls="exa --icons"
alias ll="exa -lah --icons"
alias q="exit"
alias c="clear"
# Aliases:1 ends here

# [[file:Console.org::*Starship prompt][Starship prompt:1]]
eval "$(starship init bash)"
# Starship prompt:1 ends here
