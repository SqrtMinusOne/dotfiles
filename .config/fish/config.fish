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
# Fish:2 ends here

# [[file:../../Console.org::*Fish][Fish:4]]
if ! test -n "$TMUX"; and ! test -n "$IS_EMACS";
    colorscript random
end
# Fish:4 ends here
