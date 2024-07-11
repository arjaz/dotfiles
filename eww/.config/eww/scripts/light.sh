#!/bin/sh

if [ -f ~/.config/eww/eww-light.scss ]
then
    mv ~/.config/eww/eww.scss ~/.config/eww/eww-dark.scss && mv ~/.config/eww/eww-light.scss ~/.config/eww/eww.scss
else
    echo "nok"
fi
