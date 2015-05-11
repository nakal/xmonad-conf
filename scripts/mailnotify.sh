#!/bin/sh

MAIL_COUNT=$1
SCREENPOS_X=$2
SCREENPOS_Y=$3

xscreensaver-command -time | grep -qv ' screen locked since '

if [ $? -eq 0 ] && [ "$MAIL_COUNT" -gt 0 ]; then
	echo "O0a16" > /dev/speaker
	if [ -n "$SCREENPOS_X" ] && [ -n "$SCREENPOS_Y" ]; then
		echo "New mail" | dzen2 -p 2 -fn 'Inconsolata 13:bold' -bg darkblue -fg white -e '' -ta c -x "$SCREENPOS_X" -y "$SCREENPOS_Y" -w 100 -h 30
	fi
fi

exit 0
