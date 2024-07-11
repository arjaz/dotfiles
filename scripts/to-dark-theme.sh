#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark' &&
    feh --bg-fill --no-fehbg ~/pictures/wallpapers/frieren-anime-girl-3840x2160-15156.jpg &&
    sh ~/dotfiles/polybar/.config/polybar/dark.sh &&
    sh ~/dotfiles/rofi/.config/rofi/dark.sh
