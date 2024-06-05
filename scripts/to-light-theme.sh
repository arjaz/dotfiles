#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-light' &&
    feh --bg-fill --no-fehbg ~/pictures/wallpapers/cropped-2560-1440-1342229.png &&
    sh ~/dotfiles/.config/eww/scripts/light.sh &&
    sh ~/dotfiles/.config/rofi/light.sh
