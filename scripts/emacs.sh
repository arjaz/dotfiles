#!/usr/bin/env sh
command="$(echo -e "dired\nmu4e\neshell" | dmenu )"

case $command in
    mu4e)
        emacsclient -c -a '' --eval '(mu4e)'
        ;;
    dired)
        emacsclient -c -a '' --eval '(dired ".")'
        ;;
    eshell)
        emacsclient -c -a '' --eval '(eshell)'
        ;;
esac
