#!/usr/bin/env bash
wget -q --spider http://gnu.org
if [ $? -eq 0 ]; then
	/bin/rsync -avz uio:~/privat/ /home/sully/dev/uio
fi
