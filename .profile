# [[file:Console.org::*Environment][Environment:1]]
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
# Environment:1 ends here

# [[file:Console.org::*Environment][Environment:2]]
export RIPGREP_CONFIG_PATH=$HOME/.config/ripgrep/ripgreprc
# Environment:2 ends here

# [[file:Console.org::*Environment][Environment:3]]
export LEDGER_FILE="$HOME/30-39 Life/32 org-mode/ledger/ledger.journal"
# Environment:3 ends here

# [[file:Console.org::*Environment][Environment:4]]
if command -v termux-setup-storage > /dev/null; then
    export IS_ANDROID=true
    [[ -f ~/.android_profile ]] && . ~/.android_profile
fi
# Environment:4 ends here

# [[file:Console.org::*Environment][Environment:5]]
# TZ='Asia/Karachi'; export TZ
# Environment:5 ends here

# [[file:Console.org::*My paths][My paths:1]]
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
    export PATH="$HOME/bin/scripts:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    export PATH="$HOME/.local/bin:$PATH"
fi
# My paths:1 ends here

#!/usr/bin/env bash
# [[file:Console.org::*ssh-agent][ssh-agent:3]]
export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket
# ssh-agent:3 ends here

# [[file:Console.org::*Misc settings][Misc settings:1]]
export JUPYTER_CONFIG_DIR=$HOME/.config/jupyter
# Misc settings:1 ends here

# [[file:Console.org::*Misc settings][Misc settings:2]]
export GIO_EXTRA_MODULES=""
# Misc settings:2 ends here

# [[file:Console.org::*Misc settings][Misc settings:3]]
export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus
# Misc settings:3 ends here

# [[file:Console.org::*Other package managers][Other package managers:1]]
if [ -d "$HOME/.cask" ]; then
    export PATH="/home/pavel/.cask/bin:$PATH"
fi
# Other package managers:1 ends here

# [[file:Console.org::*Other package managers][Other package managers:2]]
if [ -d "$HOME/.local/share/flatpak" ]; then
    export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share"
fi
# Other package managers:2 ends here

# [[file:Console.org::*Other package managers][Other package managers:3]]
if [ -f /run/current-system/profile/etc/profile.d/nix.sh ] && [ -z "$NO_GUIX" ] ; then
  . /run/current-system/profile/etc/profile.d/nix.sh
fi

if [ -e /home/pavel/.nix-profile/etc/profile.d/nix.sh ] && [ -z "$NO_GUIX" ] ; then . /home/pavel/.nix-profile/etc/profile.d/nix.sh; fi
# Other package managers:3 ends here

# [[file:Console.org::*Other package managers][Other package managers:4]]
if [ -d "$HOME/bin/gradle/gradle-9.0.0" ]; then
    export PATH="$HOME/bin/gradle/gradle-9.0.0/bin:$PATH"
fi
# Other package managers:4 ends here

# [[file:Console.org::*Other package managers][Other package managers:5]]
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
# Other package managers:5 ends here

# [[file:Console.org::*XResources][XResources:1]]
if [ -z "$IS_ANDROID" ]; then
    xrdb ~/.Xresources
fi
# XResources:1 ends here
