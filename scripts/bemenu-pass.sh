#!/usr/bin/env bash

MENU="~/dotfiles/scripts/bemenu.sh -p '󰌆 '"

PASSWORD_STORE_DIR="${PASSWORD_STORE_DIR:-$HOME/.password-store}"

# Detect clipboard tool
if [ -n "$WAYLAND_DISPLAY" ]; then
    CLIP="wl-copy"
    CLEAR_CLIP="wl-copy --clear"
else
    CLIP="xclip -selection clipboard"
    CLEAR_CLIP="echo -n | xclip -selection clipboard"
fi

# Get password entries
entries=$(find "$PASSWORD_STORE_DIR" -type f -name '*.gpg' 2>/dev/null | \
          sed -e "s|^$PASSWORD_STORE_DIR/||" -e 's/\.gpg$//' | sort)

# Main selection
selected=$(echo "$entries" | eval "$MENU")

[ -z "$selected" ] && exit 0

copy_and_clear() {
    echo -n "$1" | $CLIP
    (sleep 45; $CLEAR_CLIP) &>/dev/null &
}

# Prefer OTP
if otp=$(pass otp "$selected" 2>/dev/null); then
    copy_and_clear "$otp"
    exit 0
fi

# Fallback to password
pass show -c "$selected" 2>/dev/null

