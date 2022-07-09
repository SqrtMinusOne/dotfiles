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
alias gg="lazygit"
alias ls="exa --icons"
alias ll="exa -lah --icons"
alias q="exit"
alias c="clear"
alias ci="init_conda"
alias ca="conda activate"
alias cii="export INIT_CONDA=true && init_conda"
# Aliases:1 ends here

# [[file:Console.org::*Aliases][Aliases:2]]
if [[ ! -z "$SIMPLE" ]]; then
    unalias ls
    alias ll="ls -lah"
fi
# Aliases:2 ends here

# [[file:Console.org::*Anaconda][Anaconda:1]]
init_conda () {
    __conda_setup="$('/home/pavel/.guix-extra-profiles/dev/dev/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        if [ -f "/home/pavel/.guix-extra-profiles/dev/dev/etc/profile.d/conda.sh" ]; then
            . "/home/pavel/.guix-extra-profiles/dev/dev/etc/profile.d/conda.sh"
        else
            # export PATH="/home/pavel/Programs/miniconda3/bin:$PATH"
            echo "what"
        fi
    fi
    unset __conda_setup
}

if [[ ! -z "$INIT_CONDA" ]]; then
    init_conda
fi
# Anaconda:1 ends here

# [[file:Console.org::*Starship][Starship:1]]
if [[ -z "$SIMPLE" && "$TERM" != "dumb" ]]; then
    eval "$(starship init bash)"
fi
# Starship:1 ends here
