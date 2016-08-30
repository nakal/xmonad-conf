#!/bin/sh

MYDIR=`dirname $0`
. $MYDIR/include.sh

if [ -z "$@" ]; then
       exec "$TERMINAL" "$CLASS_OPT" "xmDialog" -e $0 x
fi

CACHE_FILE="$CACHE_DIR/ssh-last.txt"

DEFAULT=`test -r "$CACHE_FILE" && cat "$CACHE_FILE" | awk '!seen[$0]++' | tail -n 10`

LOGINS=""
SELECTED="on"
for l in $DEFAULT; do
	LOGINS="$LOGINS $l $SELECTED"
	SELECTED="off"
done

TEMPFILE=`mktemp -t xmonad-script-output`
CANCELLED=0
dialog --no-items --radiolist "SSH connection:" 50 70 45 $LOGINS 2> "$TEMPFILE" || CANCELLED=1
sleep 2
INPUT=`cat "$TEMPFILE"`

if [ $CANCELLED -ne 0 ]; then
	CANCELLED=0
	dialog --inputbox "New SSH connection:\n[user@]hostname[:port]?" 20 40 2> "$TEMPFILE" || CANCELLED=1
	INPUT=`cat "$TEMPFILE"`
	if [ $CANCELLED -ne 0 ]; then
		rm -f "$TEMPFILE"
		exit 0
	fi
fi
rm -f "$TEMPFILE"

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

ssh $SSH_OPT "$SSH_LOGIN"
