#!/usr/bin/env sh

url=$(xclip -o -selection clipboard)
notify-send "Downloading" "$url"

cd ~/videos/download/ || exit 1

error=$(yt-dlp --no-playlist "$url" 2>&1 >/dev/null)
status=$?

if [ $status -eq 0 ]; then
    notify-send "Download complete" "$url"
else
    notify-send "Download failed" "$error"
fi
