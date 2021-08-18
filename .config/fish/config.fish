# [[file:../../Console.org::*Fish][Fish:1]]
starship init fish | source
# Fish:1 ends here

# [[file:../../Console.org::*Fish][Fish:2]]
fish_vi_key_bindings

alias v="vim"
alias gg="lazygit"
alias ls="exa --icons"
alias ll="exa -lah --icons"
alias q="exit"
alias c="clear"
alias ci="init_conda"
alias ca="conda activate"
alias cii="export INIT_CONDA=true && init_conda"
# Fish:2 ends here

# [[file:../../Console.org::*Fish][Fish:3]]
if ! test -n "$TMUX"; and ! test -n "$IS_EMACS";
    colorscript random
end
# Fish:3 ends here

# [[file:../../Console.org::*Fish][Fish:4]]
set fish_greeting
# Fish:4 ends here

# [[file:../../Console.org::*Anaconda][Anaconda:1]]
function init_conda
    eval /home/pavel/.guix-extra-profiles/dev/dev/bin/conda "shell.fish" "hook" $argv | source
end

if test -n "$INIT_CONDA";
    init_conda
end
# Anaconda:1 ends here

# [[file:../../Console.org::*Anaconda][Anaconda:2]]
if test -n "$EMACS_CONDA_ENV";
    conda activate $EMACS_CONDA_ENV
end
# Anaconda:2 ends here

# [[file:../../Console.org::*Colors][Colors:1]]
set fish_color_command cyan
set fish_color_comment green
set fish_color_end white
set fish_color_error red
set fish_color_escape yellow
set fish_color_operator yellow
set fish_color_param magenta
set fish_color_quote brwhite
set fish_color_redirection yellow
# Colors:1 ends here

# [[file:../../Console.org::*Keybindings][Keybindings:1]]
bind -M insert \el forward-char
bind -M insert \eh backward-char
bind -M insert \ew forward-word
bind -M insert \eb backward-word
# Keybindings:1 ends here
