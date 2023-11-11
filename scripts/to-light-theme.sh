#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-light' &&
    feh --bg-fill --no-fehbg ~/pictures/wallpapers/wallhaven-96jdd8_2560x1440.png &&
    sh ~/.config/kitty/light.sh &&
    sh ~/.config/polybar/light.sh &&
    sh ~/dotfiles/rcs/eww/scripts/light.sh &&
    sh ~/.config/rofi/light.sh
