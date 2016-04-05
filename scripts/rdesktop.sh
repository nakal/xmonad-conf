#!/bin/sh

CACHE_DIR="$HOME/.cache/xmonad-conf"
CACHE_FILE="$CACHE_DIR/rdesktop-last.txt"

DEFAULT=`test -r "$CACHE_FILE" && cat "$CACHE_FILE" | awk '!seen[$0]++' | tail -n 10`

LOGINS=""
SELECTED="TRUE"
for l in $DEFAULT; do
	LOGINS="$LOGINS $SELECTED $l"
	SELECTED="FALSE"
done

INPUT=`zenity --list --radiolist --column "" --column "Logins" $LOGINS`
if [ $? -ne 0 ]; then
	INPUT=`zenity --entry --text "New rdesktop connection:\n[user@]hostname[:port]?"`
	if [ $? -ne 0 ]; then
		exit 0
	fi
fi

USER_OPT=`echo "$INPUT" | sed 's/^[^@]*$//; s/\(.*\)@.*/\1/; /^$/q ; s/^/-u /'`
HOSTNAME=`echo "$INPUT" | sed 's/^.*@//'`

if [ -z "$HOSTNAME" ]; then
	exit 0
fi

mkdir -p "$CACHE_DIR" && ( echo "$INPUT" > "$CACHE_FILE" ;
		for l in $DEFAULT; do
			if [ "$l" != "$INPUT" ]; then
				echo "$l" >> "$CACHE_FILE"
			fi
		done )

exec rdesktop $USER_OPT -g 1600x960 -K "$HOSTNAME"
