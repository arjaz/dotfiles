#!/usr/bin/env sh
cd ~/videos/download
url=$(xclip -o -selection clipboard)
notify-send Downloading $url
yt-dlp $url && notify-send "Finished yt-dlp" $url || notify-send "Failed yt-dlp" $url
