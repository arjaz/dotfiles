!#/bin/env sh

if type "xrandr"; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    MONITOR=$m polybar --reload arjaz &
  done
else
  polybar --reload example &
fi
