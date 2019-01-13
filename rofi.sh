#!/bin/sh
bg_color=#bb2f343f
text_color=#f3f4f5
htext_color=#9575cd

rofi -show run -eh 2 -padding 400 -fullscreen -color-window "$bg_color, $bg_color, $bg_color" -color-normal "$bg_color, $text_color, $bg_color, $bg_color, $htext_color" -font "Roboto 14"
