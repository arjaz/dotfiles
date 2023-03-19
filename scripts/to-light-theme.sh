#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-light' &&
    feh --bg-fill --no-fehbg ~/pictures/wallpapers/wallhaven-96jdd8_1920x1080.png &&
    sh ~/.config/kitty/light.sh &&
    sh ~/.config/polybar/light.sh &&
    sh ~/.config/rofi/light.sh
