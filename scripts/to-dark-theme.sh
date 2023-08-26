#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark' &&
    feh --bg-fill --no-fehbg ~/pictures/wallpapers/wallhaven-dg3kj3_4x.png &&
    sh ~/.config/kitty/dark.sh &&
    sh ~/.config/polybar/dark.sh &&
    sh ~/dotfiles/rcs/eww/scripts/dark.sh
    # sh ~/.config/rofi/dark.sh
