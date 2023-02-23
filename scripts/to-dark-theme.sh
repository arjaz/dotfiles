#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark' &&
    sh ~/.config/polybar/dark.sh &&
    sh ~/.config/rofi/dark.sh &&
    sh ~/.config/kitty/dark.sh &&
    feh --bg-fill ~/Pics/wallpapers/wallhaven-gjkjwe_1920x1080.png
