#!/usr/bin/env sh
cd ~/videos/download
url=$(xclip -o -selection clipboard)
# TODO: check if valid url
notify-send Downloading $url
yt-dlp $url && notify-send Finished $url || notify-send Failed $url
