#!/bin/sh

MYDIR=`dirname $0`
. $MYDIR/include.sh

if [ -z "$@" ]; then
	TEMPFILE=`mktemp -t xmonad-script-output`
	"$TERMINAL" "$CLASS_OPT" "xmDialog" -e $0 "$TEMPFILE"
	EXEC=`cat $TEMPFILE`
	rm -f "$TEMPFILE"
	if [ -z "$EXEC" ]; then
		exit 0
	fi
	exec sh -c "$EXEC"
fi

TEMPFILE="$1"
CACHE_FILE="$CACHE_DIR/ssh-last.txt"

DEFAULT=`test -r "$CACHE_FILE" && cat "$CACHE_FILE" | awk '!seen[$0]++' | tail -n 10`

LOGINS=""
SELECTED="on"
for l in $DEFAULT; do
	LOGINS="$LOGINS $l $SELECTED"
	SELECTED="off"
done

CANCELLED=0
dialog --no-items --radiolist "SSH connection:" 50 70 45 $LOGINS "Add a new entry" off 2> "$TEMPFILE" || CANCELLED=1
INPUT=`cat "$TEMPFILE"`

if [ $CANCELLED -ne 0 ]; then
	exit 1
fi

if [ "$INPUT" = "Add a new entry" ]; then
	CANCELLED=0
	dialog --inputbox "New SSH connection:\n[user@]hostname[:port]?" 20 40 2> "$TEMPFILE" || CANCELLED=1
	INPUT=`cat "$TEMPFILE"`
	if [ $CANCELLED -ne 0 ]; then
		rm -f "$TEMPFILE"
		exit 1
	fi
fi

SSH_LOGIN=`echo "$INPUT" | sed 's/:[^:]*$//'`
SSH_OPT=`echo "$INPUT" | grep ':' | sed 's/^.*://; /^$/q; s/^/-p /'`

if [ -z "$SSH_LOGIN" ]; then
	exit 1
fi

mkdir -p "$CACHE_DIR" && ( echo "$INPUT" > "$CACHE_FILE" ;
		for l in $DEFAULT; do
			if [ "$l" != "$INPUT" ]; then
				echo "$l" >> "$CACHE_FILE"
			fi
		done )

echo "$DEFAULT_X_TERMINAL -e ssh -t $SSH_OPT '$SSH_LOGIN' 'tmux -2 new-session'" > "$TEMPFILE"
exit 0
