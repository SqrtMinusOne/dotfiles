starship init fish | source

fish_vi_key_bindings

alias c="clear"
alias v="nvim"
alias gg="lazygit"
alias ls="exa --icons"
alias ll="exa -lah --icons"
alias q="exit"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /home/pavel/Programs/miniconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

if ! test -n "$TMUX"; and ! test -n "$IS_EMACS";
    colorscript random
end