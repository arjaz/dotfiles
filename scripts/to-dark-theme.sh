#!/usr/bin/env sh
gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark' &&
    feh --bg-fill --no-fehbg ~/pictures/wallpapers/brncgv8xuoac1.webp &&
    sh ~/dotfiles/.config/kitty/dark.sh &&
    sh ~/dotfiles/.config/eww/scripts/dark.sh &&
    sh ~/dotfiles/.config/rofi/dark.sh
