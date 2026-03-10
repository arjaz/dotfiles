#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-light' && \
    # feh --bg-fill --no-fehbg ~/pictures/halftone-wallhaven-qzv6rd_2560x1440.png && \
    feh --bg-fill --no-fehbg ~/pictures/wallhaven-qzv6rd_2560x1440.png && \
    # feh --bg-fill --no-fehbg ~/pictures/halftone-wallhaven-gpl8d3_2560x1440.png && \
    # feh --bg-fill --no-fehbg ~/pictures/wallhaven-gpl8d3_2560x1440.png && \
    # feh --bg-fill --no-fehbg ~/pictures/7ndi66_dithered.png && \
    sh ~/dotfiles/polybar/.config/polybar/light.sh && \
    sh ~/dotfiles/rofi/.config/rofi/light.sh
