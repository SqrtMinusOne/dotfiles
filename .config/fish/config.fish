# [[file:../../Console.org::*Fish][Fish:1]]
if [ "$TERM" != "dumb" ]; and type -q starship;
    starship init fish | source
else
    function fish_prompt -d "Write out the prompt"
        printf '%s@%s %s%s%s > ' $USER $hostname \
            (set_color $fish_color_cwd) (basename (pwd)) (set_color normal)
    end
end
# Fish:1 ends here

# [[file:../../Console.org::*Fish][Fish:2]]
fish_vi_key_bindings

alias q="exit"
alias c="clear"
if type -q exa
    alias ls="exa --icons"
    alias ll="exa -lah --icons"
else
    alias ll="ls -h"
end
# Fish:2 ends here

# [[file:../../Console.org::*Fish][Fish:3]]
if ! test -n "$TMUX"; and ! test -n "$IS_EMACS"; and type -q colorscript
    colorscript random
end
# Fish:3 ends here

# [[file:../../Console.org::*Fish][Fish:4]]
set fish_greeting
# Fish:4 ends here

# [[file:../../Console.org::*Micromamba][Micromamba:1]]
function init_mamba
    set -gx MAMBA_EXE "/gnu/store/w0rrglxs2247nr4wawrh5dylisjra1q4-micromamba-bin-1.4.4-0/bin/micromamba"
    set -gx MAMBA_ROOT_PREFIX "/home/pavel/micromamba"
    $MAMBA_EXE shell hook --shell fish --prefix $MAMBA_ROOT_PREFIX | source
end

if test -n "$INIT_MAMBA";
    init_mamba
end

alias ca="micromamba activate"
alias ci="init_mamba"
alias cii="export INIT_MAMBA=true && init_mamba"
# Micromamba:1 ends here

# [[file:../../Console.org::*Micromamba][Micromamba:2]]
# if test -n "$EMACS_CONDA_ENV";
    # conda activate $EMACS_CONDA_ENV
# end
# Micromamba:2 ends here

# [[file:../../Console.org::*Colors][Colors:1]]
set fish_color_command cyan
set fish_color_comment green
set fish_color_end white
set fish_color_error red
set fish_color_escape yellow
set fish_color_operator yellow
set fish_color_param magenta
set fish_color_quote green
set fish_color_redirection yellow
# Colors:1 ends here

# [[file:../../Console.org::*Keybindings][Keybindings:1]]
bind -M insert \el forward-char
bind -M insert \eh backward-char
bind -M insert \ew forward-word
bind -M insert \eb backward-word
# Keybindings:1 ends here

# [[file:../../Console.org::*Functions][Functions:1]]
function e
    eval $EDITOR $argv
end
# Functions:1 ends here

# [[file:../../Console.org::*direnv][direnv:1]]
direnv hook fish | source
# direnv:1 ends here
