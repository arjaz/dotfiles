#!/bin/sh

if [ -f ~/dotfiles/rcs/eww/eww-dark.scss ]
then
    mv ~/dotfiles/rcs/eww/eww.scss ~/dotfiles/rcs/eww/eww-light.scss && mv ~/dotfiles/rcs/eww/eww-dark.scss ~/dotfiles/rcs/eww/eww.scss
else
    echo "nok"
fi
