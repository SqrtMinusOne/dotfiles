# [[file:Console.org::*Environment][Environment:1]]
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
# Environment:1 ends here

# [[file:Console.org::*Environment][Environment:2]]
export RIPGREP_CONFIG_PATH=$HOME/.config/ripgrep/ripgreprc
# Environment:2 ends here

# [[file:Console.org::*Environment][Environment:3]]
export LEDGER_FILE=~/Documents/org-mode/ledger/ledger.journal
# Environment:3 ends here

# [[file:Console.org::*My paths][My paths:1]]
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
    export PATH="$HOME/bin/scripts:$PATH"
fi
# My paths:1 ends here

# [[file:Console.org::*Guix settings][Guix settings:1]]
if [ -z "$IS_ANDROID" ]; then
    GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
    for i in $GUIX_EXTRA_PROFILES/*; do
        profile=$i/$(basename "$i")
        if [ -f "$profile"/etc/profile ]; then
            GUIX_PROFILE="$profile"
            . "$GUIX_PROFILE"/etc/profile
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
if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then
  . /run/current-system/profile/etc/profile.d/nix.sh
fi
# Other package managers:3 ends here

# [[file:Console.org::*Other package managers][Other package managers:4]]
if [ -d "$HOME/.guix-extra-profiles/desktop-misc" ]; then
    export FONTCONFIG_PATH="$HOME/.guix-extra-profiles/desktop-misc/desktop-misc/etc/fonts"
fi
# Other package managers:4 ends here

# [[file:Console.org::*Other package managers][Other package managers:5]]
if [ -d "$HOME/.nix-profile" ]; then
    export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.nix-profile/share/applications"
fi
# Other package managers:5 ends here

# [[file:Console.org::*npm][npm:2]]
export NPM_CONFIG_USERCONFIG=$HOME/._npmrc
# npm:2 ends here

# [[file:Console.org::*npm][npm:3]]
NPM_PACKAGES="${HOME}/.npm-packages"

export PATH="$PATH:$NPM_PACKAGES/bin"
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"
# npm:3 ends here

# [[file:Console.org::*XResources][XResources:1]]
if [ -z "$IS_ANDROID" ]; then
    xrdb ~/.Xresources
fi
# XResources:1 ends here
