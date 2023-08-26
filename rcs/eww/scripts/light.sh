#!/bin/sh

if [ -f ~/dotfiles/rcs/eww/eww-light.scss ]
then
    mv ~/dotfiles/rcs/eww/eww.scss ~/dotfiles/rcs/eww/eww-dark.scss && mv ~/dotfiles/rcs/eww/eww-light.scss ~/dotfiles/rcs/eww/eww.scss
else
    echo "nok"
fi
