#!/bin/sh

if [ -n "$BEMENU_RUN" ]; then
    target="bemenu-run"
else
    target="bemenu"
fi

exec "$target" \
    -l 5 \
    -i \
    --fn "IoskeleyMono 16" \
    --tb "#000000" --tf "#ffffff" \
    --fb "#000000" --ff "#ffffff" \
    --cb "#000000" --cf "#ffffff" \
    --nb "#000000" --nf "#ffffff" \
    --fbb "#000000" --fbf "#ffffff" \
    --sb "#000000" --sf "#ffffff" \
    --ab "#000000" --af "#ffffff" \
    --hb "#000000" --hf "#e0f6e0" \
    "$@"
