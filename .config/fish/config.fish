# [[file:../../Shell.org::*Fish][Fish:1]]
starship init fish | source
# Fish:1 ends here

# [[file:../../Shell.org::*Fish][Fish:2]]
fish_vi_key_bindings

alias v="vim"
alias gg="lazygit"
alias ls="exa --icons"
alias ll="exa -lah --icons"
alias q="exit"
alias c="clear"
# Fish:2 ends here

# [[file:../../Shell.org::*Fish][Fish:3]]
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /home/pavel/Programs/miniconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<
# Fish:3 ends here

# [[file:../../Shell.org::*Fish][Fish:4]]
if ! test -n "$TMUX"; and ! test -n "$IS_EMACS";
    colorscript random
end
# Fish:4 ends here
