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

#!/usr/bin/env bash
# [[file:Console.org::*Environment][Environment:6]]
if [ -f "/home/pavel/.no-guix" ]; then
    export NO_GUIX=true
    export PATH=$(echo $PATH | tr ":" "\n" | grep -vE "guix|nix|gnu" | tr "\n" ":")
fi
# Environment:6 ends here

# [[file:Console.org::*Environment][Environment:7]]
if [ "$(hostname)" = "amaranth" ]; then
    export XDG_DATA_DIRS=/usr/share/fkms:/usr/local/share:/usr/share/raspi-ui-overrides:/usr/share:/usr/share/gdm:/var/lib/menu-xdg:$XDG_DATA_DIRS
fi
# Environment:7 ends here

# [[file:Console.org::*My paths][My paths:1]]
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
    export PATH="$HOME/bin/scripts:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    export PATH="$HOME/.local/bin:$PATH"
fi
# My paths:1 ends here

# [[file:Console.org::*SSL Certs][SSL Certs:1]]
if [ -d "$HOME/.guix-extra-profiles" ] ; then
    export SSL_CERT_DIR="$HOME/.guix-extra-profiles/system/system/etc/ssl/certs/"
    export SSL_CERT_FILE="$HOME/.guix-extra-profiles/system/system/etc/ssl/certs/ca-certificates.crt"
    export GIT_SSL_CAINFO="$SSL_CERT_FILE"
    export CURL_CA_BUNDLE="$SSL_CERT_FILE"
fi
# SSL Certs:1 ends here

# [[file:Console.org::*ssh-agent][ssh-agent:1]]
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent -t 1h > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [[ ! -f "$SSH_AUTH_SOCK" ]]; then
    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
fi
# ssh-agent:1 ends here

# [[file:Console.org::*Guix settings][Guix settings:1]]
if [ -z "$IS_ANDROID" ] && [ -z "$NO_GUIX" ] ; then
    GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
    for i in $GUIX_EXTRA_PROFILES/*; do
        profile=$i/$(basename "$i")
        if [ -f "$profile"/etc/profile ]; then
            GUIX_PROFILE="$profile"
            . "$GUIX_PROFILE"/etc/profile
        fi
        if [ -d "$profile"/share/man ]; then
            if command -v manpath >/dev/null 2>/dev/null; then
                export MANPATH="${MANPATH:-$(manpath)}:$profile/share/man"
            else
                export MANPATH="${MANPATH}:$profile/share/man"
            fi
        fi
        export XDG_DATA_DIRS="$XDG_DATA_DIRS:$profile/share"
        unset profile
    done
fi
# Guix settings:1 ends here

# [[file:Console.org::*Guix settings][Guix settings:2]]
export JUPYTER_CONFIG_DIR=$HOME/.config/jupyter
# Guix settings:2 ends here

# [[file:Console.org::*Guix settings][Guix settings:3]]
export GUIX_PACKAGE_PATH=~/guix-packages
# Guix settings:3 ends here

# [[file:Console.org::*Guix settings][Guix settings:4]]
export GUIX_LOCPATH=$HOME/.guix-extra-profiles/console/console/lib/locale
# Guix settings:4 ends here

# [[file:Console.org::*Guix settings][Guix settings:5]]
export GIO_EXTRA_MODULES=""
# Guix settings:5 ends here

# [[file:Console.org::*Other package managers][Other package managers:1]]
if [ -d "$HOME/.cask" ]; then
    export PATH="/home/pavel/.cask/bin:$PATH"
fi
# Other package managers:1 ends here

# [[file:Console.org::*Other package managers][Other package managers:2]]
if [ -d "/usr/local/go/" ]; then
    export PATH="/usr/local/go/bin:$PATH"
fi
# Other package managers:2 ends here

# [[file:Console.org::*Other package managers][Other package managers:3]]
if [ -d "$HOME/.cargo" ] ; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi
# Other package managers:3 ends here

# [[file:Console.org::*Other package managers][Other package managers:4]]
if [ -d "/opt/guile" ]; then
    export PATH="/opt/guile/bin:$PATH"
fi
# Other package managers:4 ends here

# [[file:Console.org::*Other package managers][Other package managers:5]]
if [ -d "$HOME/.local/share/flatpak" ]; then
    export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share"
fi
# Other package managers:5 ends here

# [[file:Console.org::*Other package managers][Other package managers:6]]
if [ -f /run/current-system/profile/etc/profile.d/nix.sh ] && [ -z "$NO_GUIX" ] ; then
  . /run/current-system/profile/etc/profile.d/nix.sh
fi

if [ -e /home/pavel/.nix-profile/etc/profile.d/nix.sh ] && [ -z "$NO_GUIX" ] ; then . /home/pavel/.nix-profile/etc/profile.d/nix.sh; fi
# Other package managers:6 ends here

# [[file:Console.org::*Other package managers][Other package managers:7]]
if [ -d "$HOME/.guix-extra-profiles/desktop-misc" ] && [ -z "$NO_GUIX" ] ; then
    export FONTCONFIG_PATH="$HOME/.guix-extra-profiles/desktop-misc/desktop-misc/etc/fonts"
fi
# Other package managers:7 ends here

# [[file:Console.org::*Other package managers][Other package managers:8]]
if [ -d "$HOME/.nix-profile" ] && [ -z "$NO_GUIX" ]; then
    export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.nix-profile/share/applications"
fi
# Other package managers:8 ends here

# [[file:Console.org::*npm][npm:2]]
export NPM_CONFIG_USERCONFIG=$HOME/._npmrc
# npm:2 ends here

# [[file:Console.org::*npm][npm:3]]
NPM_PACKAGES="${HOME}/.npm-packages"

export PATH="$PATH:$NPM_PACKAGES/bin"
export MANPATH="${MANPATH:-$(manpath)}:$NPM_PACKAGES/share/man"
# npm:3 ends here

# [[file:Console.org::*XResources][XResources:1]]
if [ -z "$IS_ANDROID" ]; then
    xrdb ~/.Xresources
fi
# XResources:1 ends here
