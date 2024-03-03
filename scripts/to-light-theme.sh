#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-light' &&
    feh --bg-fill --no-fehbg ~/pictures/wallpapers/brncgv8xuoac1.webp &&
    sh ~/dotfiles/.config/kitty/light.sh &&
    sh ~/dotfiles/.config/eww/scripts/light.sh &&
    sh ~/dotfiles/.config/rofi/light.sh
