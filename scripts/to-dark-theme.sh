#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark' &&
    feh --bg-fill --no-fehbg ~/pictures/wallpapers/wallhaven-6oypll_2560x1440.png &&
    sh ~/.config/kitty/dark.sh &&
    sh ~/.config/polybar/dark.sh
    # sh ~/.config/rofi/dark.sh
