#!/usr/bin/env bash

# TODO: support OTP

# Configure menu tool
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

# Parse password file
content=$(pass show "$selected" 2>/dev/null)
fields=("password: $(head -n1 <<< "$content")")

while read -r line; do
    [[ $line =~ ^([^:]+):\ (.*) ]] && fields+=("${BASH_REMATCH[1]}: ${BASH_REMATCH[2]}")
done < <(tail -n +2 <<< "$content")

# Field selection
field=$(printf "%s\n" "${fields[@]%%:*}" | eval "$MENU -p ''")

[ -z "$field" ] && exit 0

# Extract and copy value
case $field in
    "password")
        pass show -c "$selected" 2>/dev/null
        ;;
    *)
        value=$(grep "^${field}:" <<< "$content" | cut -d: -f2- | xargs)
        echo -n "$value" | $CLIP
        # Clear clipboard after 45 seconds
        (sleep 45; $CLEAR_CLIP) &>/dev/null &
        ;;
esac
