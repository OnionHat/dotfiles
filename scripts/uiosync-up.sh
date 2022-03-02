#!/usr/bin/env bash
while inotifywait -r -e modify,create,delete,move ~/dev/uio/; do
	wget -q --spider http://gnu.org
	if [ $? -eq 0 ]; then
		/bin/rsync -avz --delete ~/dev/uio/ uio:~/privat
	fi
done
