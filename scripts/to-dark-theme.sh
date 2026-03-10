#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark' && \
    # feh --bg-fill --no-fehbg ~/pictures/halftone-wallhaven-gpvq63_2560x1440.png && \
    feh --bg-fill --no-fehbg ~/pictures/halftone-wallhaven-qzv6rd_2560x1440.png && \
    sh ~/dotfiles/polybar/.config/polybar/dark.sh && \
    sh ~/dotfiles/rofi/.config/rofi/dark.sh
