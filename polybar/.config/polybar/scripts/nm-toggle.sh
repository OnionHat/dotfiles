#!/bin/sh
inter=enp4s0
state=1
if [[ `ip link show $inter` == *"DOWN"* ]]; then state=0; fi
echo $state
