# [[file:Console.org::*Environment][Environment:1]]
# export EDITOR=/usr/bin/vim
# export BROWSER=/usr/bin/firefox
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
# export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
# Environment:1 ends here

# [[file:Console.org::*My paths][My paths:1]]
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
    export PATH="$HOME/bin/scripts:$PATH"
fi
# My paths:1 ends here

# [[file:Console.org::*Guix settings][Guix settings:1]]
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
# Guix settings:1 ends here

# [[file:Console.org::*Guix settings][Guix settings:2]]
export JUPYTER_CONFIG_DIR=$HOME/.config/jupyter
# Guix settings:2 ends here

# [[file:Console.org::*Guix settings][Guix settings:3]]
export GUIX_PACKAGE_PATH=~/guix-packages
# Guix settings:3 ends here

# [[file:Console.org::*Other package managers][Other package managers:1]]
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share"
# Other package managers:1 ends here

# [[file:Console.org::*Other package managers][Other package managers:2]]
if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then
  . /run/current-system/profile/etc/profile.d/nix.sh
fi
# Other package managers:2 ends here

# [[file:Console.org::*Other package managers][Other package managers:3]]
if [ -d "$HOME/.guix-extra-profiles/desktop" ]; then
    export FONTCONFIG_PATH="$HOME/.guix-extra-profiles/desktop/desktop/etc/fonts"
fi
# Other package managers:3 ends here

# [[file:Console.org::*Other package managers][Other package managers:4]]
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.nix-profile/share/applications"
# Other package managers:4 ends here

# [[file:Console.org::*npm][npm:2]]
export NPM_CONFIG_USERCONFIG=$HOME/._npmrc
# npm:2 ends here

# [[file:Console.org::*npm][npm:3]]
NPM_PACKAGES="${HOME}/.npm-packages"

export PATH="$PATH:$NPM_PACKAGES/bin"
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"
# npm:3 ends here

# [[file:Console.org::*XResources][XResources:1]]
xrdb ~/.Xresources
# XResources:1 ends here
