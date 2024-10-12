#!/bin/bash

dbus-monitor --session "type='signal',interface='org.freedesktop.portal.Settings',member='SettingChanged'" | \
while read -r line; do
    if [[ "$line" =~ "color-scheme" ]]; then
        read -r next_line
        if [[ "$next_line" =~ uint32\ 1 ]]; then
            feh --bg-fill --no-fehbg ~/pictures/wallpapers/frieren-anime-girl-3840x2160-15156.jpg
            sh ~/dotfiles/polybar/.config/polybar/dark.sh
            sh ~/dotfiles/rofi/.config/rofi/dark.sh
            kitty +kitten themes --reload-in=all --cache-age=-1 Modus Vivendi
        elif [[ "$next_line" =~ uint32\ 2 ]]; then
            feh --bg-fill --no-fehbg ~/pictures/wallpapers/cropped-2560-1440-1342229.png
            sh ~/dotfiles/polybar/.config/polybar/light.sh
            sh ~/dotfiles/rofi/.config/rofi/light.sh
            kitty +kitten themes --reload-in=all --cache-age=-1 Modus Operandi Tinted
        fi
    fi
done
