#!/bin/sh

if [ -f ~/.config/eww/eww-dark.scss ]
then
    mv ~/.config/eww/eww.scss ~/.config/eww/eww-light.scss && mv ~/.config/eww/eww-dark.scss ~/.config/eww/eww.scss
else
    echo "nok"
fi
