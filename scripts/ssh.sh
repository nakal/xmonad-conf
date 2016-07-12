#!/bin/sh

TERMINAL="$1"
if [ -z "$TERMINAL" ]; then
	TERMINAL="urxvt"
fi

CACHE_DIR="$HOME/.cache/xmonad-conf"
CACHE_FILE="$CACHE_DIR/ssh-last.txt"

DEFAULT=`test -r "$CACHE_FILE" && cat "$CACHE_FILE" | awk '!seen[$0]++' | tail -n 10`

LOGINS=""
SELECTED="TRUE"
for l in $DEFAULT; do
	LOGINS="$LOGINS $SELECTED $l"
	SELECTED="FALSE"
done

INPUT=`zenity --list --radiolist --text "SSH connection:" --column "" --column "Logins" $LOGINS`
if [ $? -ne 0 ]; then
	INPUT=`zenity --entry --text "New SSH connection:\n[user@]hostname[:port]?"`
	if [ $? -ne 0 ]; then
		exit 0
	fi
fi

SSH_LOGIN=`echo "$INPUT" | sed 's/:[^:]*$//'`
SSH_OPT=`echo "$INPUT" | sed 's/^.*://; /^$/q; s/^/-p /'`

if [ -z "$SSH_LOGIN" ]; then
	exit 0
fi

mkdir -p "$CACHE_DIR" && ( echo "$INPUT" > "$CACHE_FILE" ;
		for l in $DEFAULT; do
			if [ "$l" != "$INPUT" ]; then
				echo "$l" >> "$CACHE_FILE"
			fi
		done )

exec $TERMINAL -e ssh $SSH_OPT "$SSH_LOGIN"
