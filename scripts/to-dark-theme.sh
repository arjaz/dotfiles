#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark' &&
    feh --bg-fill --no-fehbg ~/pictures/wallpapers/frieren-anime-girl-3840x2160-15156.jpg &&
    sh ~/dotfiles/.config/eww/scripts/dark.sh &&
    sh ~/dotfiles/.config/rofi/dark.sh
