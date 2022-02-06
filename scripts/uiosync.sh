#!/usr/bin/env bash
while inotifywait -r -e modify,create,delete,move ~/dev/uio/; do
    wget -q --spider http://gnu.org
    if [ $? -eq 0 ]; then
        rsync -avz ~/dev/uio/ uio:~/privat
    fi
done
