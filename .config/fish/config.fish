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
function init_conda
    eval /home/pavel/.guix-extra-profiles/dev/dev/bin/conda "shell.fish" "hook" $argv | source
end

if test -n "$INIT_CONDA";
    init_conda
end
# Fish:3 ends here

# [[file:../../Console.org::*Fish][Fish:4]]
if ! test -n "$TMUX"; and ! test -n "$IS_EMACS";
    colorscript random
end
# Fish:4 ends here

# [[file:../../Console.org::*Fish][Fish:5]]
set fish_greeting
# Fish:5 ends here
