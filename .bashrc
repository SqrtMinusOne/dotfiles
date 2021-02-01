# Return if not run interactively
[[ $- != *i* ]] && return

xhost +local:root > /dev/null 2>&1

use_fish=true

# ===================== PATHS =====================
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
    export PATH="$HOME/bin/scripts:$PATH"
fi

export MANPATH="/usr/local/texlive/2020/texmf-dist/doc/man:$MANPATH"
export INFOPATH="/usr/local/texlive/2020/texmf-dist/doc/info:$INFOPATH"
export PATH="/usr/local/texlive/2020/bin/x86_64-linux:$PATH"

if [ -d "$HOME/.cargo" ] ; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi
if [ -d "$HOME/.rvm" ] ; then
    export PATH="$PATH:$HOME/.rvm/bin"
fi
# if [ -d "$HOME/.gem" ]; then
#     export PATH="$HOME/.gem/ruby/2.7.0/bin:$PATH"
# fi
if [ -d "$HOME/go" ] ; then
    export PATH="$HOME/go/bin:$PATH"
fi
# export PATH="$HOME/.gem/ruby/2.7.0/bin/:$PATH"

[ -f "/home/pavel/.ghcup/env" ] && source "/home/pavel/.ghcup/env" # ghcup-env

if [[ $(ps --no-header --pid=$PPID --format=cmd) != "fish" && ${use_fish} ]]
then
	exec fish
fi

# ===================== COLORS =====================

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
[[ $'\n'${match_lhs} === *$'\n'"TERM "${safe_term}* ]] && use_color=true

if ${use_color} ; then
	# Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
	if type -P dircolors >/dev/null ; then
		if [[ -f ~/.dir_colors ]] ; then
			eval $(dircolors -b ~/.dir_colors)
		elif [[ -f /etc/DIR_COLORS ]] ; then
			eval $(dircolors -b /etc/DIR_COLORS)
		fi
	fi

	if [[ ${EUID} === 0 ]] ; then
		PS1='\[\033[01;31m\][\h\[\033[01;36m\] \W\[\033[01;31m\]]\$\[\033[00m\] '
	else
		PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\]]\$\[\033[00m\] '
	fi

	alias ls='ls --color=auto'
	alias grep='grep --colour=auto'
	alias egrep='egrep --colour=auto'
	alias fgrep='fgrep --colour=auto'
else
	if [[ ${EUID} === 0 ]] ; then
		# show root@ when we don't have colors
		PS1='\u@\h \W \$ '
	else
		PS1='\u@\h \w \$ '
	fi
fi

unset use_color safe_term match_lhs sh

# ===================== Settings =====================

# Sudo autocompletiong
complete -cf sudo

shopt -s checkwinsize
shopt -s expand_aliases
shopt -s autocd

# ===================== ANACONDA =====================

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/pavel/Programs/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/pavel/Programs/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/pavel/Programs/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/pavel/Programs/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


# ===================== BASH_IT =====================

# History
eval "$(starship init bash)"

shopt -s histappend
export HISTCONTROL=ignoredups:erasedups
# if [[ (! $PROMPT_COMMAND =~ .*history.*) && -z $TMUX ]]; then
#     export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"
# fi
HISTSIZE=
HISTFILESIZE=

# ===================== AUTOCOMPLETION =====================

[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion
source /usr/share/fzf/completion.bash
source /usr/share/fzf/key-bindings.bash


# ===================== ALIASES =====================
alias v="nvim"
alias gg="lazygit"
alias ls="exa --icons"
alias ll="exa -lah --icons"
alias q="exit"
alias c="clear"

