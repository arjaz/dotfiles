grep colors-dark ~/.config/polybar/config.ini &&
sed -i 's/\[colors\]/\[colors-light\]/' ~/.config/polybar/config.ini &&
sed -i 's/\[colors-dark\]/\[colors\]/'  ~/.config/polybar/config.ini
