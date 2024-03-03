#!/bin/sh

if [ -f ~/dotfiles/.config/eww/eww-light.scss ]
then
    mv ~/dotfiles/.config/eww/eww.scss ~/dotfiles/.config/eww/eww-dark.scss && mv ~/dotfiles/.config/eww/eww-light.scss ~/dotfiles/.config/eww/eww.scss
else
    echo "nok"
fi
