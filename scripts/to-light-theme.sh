#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-light' &&
    sh ~/.config/polybar/light.sh &&
    sh ~/.config/rofi/light.sh &&
    sh ~/.config/kitty/light.sh &&
    feh --bg-fill ~/Pics/wallpapers/wallhaven-96jdd8_1920x1080.png
