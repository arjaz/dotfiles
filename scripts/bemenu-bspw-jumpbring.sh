#!/bin/sh

mode="$1"
case "$mode" in
    jump|bring)
    ;;
    *)
    printf 'usage: %s [jump|bring]\n' "$0" >&2
    exit 1
    ;;
esac

windows=$(bspc query -N -n .window)

entries=$(
    for wid in $windows; do
        title=$(xtitle -t100 "$wid")
        printf '%-60s\t%s\n' "$title" "$wid"
    done
       )

chosen=$(
    printf '%s\n' "$entries" |
        ~/dotfiles/scripts/bemenu.sh -p "Window ($mode):" |
        cut -f2
      )

[ -n "$chosen" ] || exit

case "$mode" in
    jump)
        desktop=$(bspc query -D -n "$chosen")
        bspc desktop -f "$desktop"
        bspc node -f "$chosen"
        ;;
    bring)
        bspc node "$chosen" -d focused
        bspc node -f "$chosen"
        ;;
esac
