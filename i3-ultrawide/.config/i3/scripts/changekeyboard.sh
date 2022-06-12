#!/usr/bin/env bash
EN_ibus="xkb:us::eng"
NO_ibus="xkb:no:nodeadkeys:nor"
lang=`ibus engine`

if [ $lang = $EN_ibus ];then
  ibus engine $NO_ibus
  notify-send "[NO] keyboard" -t 2000 -a "System"
else
  notify-send "[EN] keyboard" -t 2000 -a "System"
  ibus engine $EN_ibus
fi
