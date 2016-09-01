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
CACHE_FILE="$CACHE_DIR/rdesktop-last.txt"

DEFAULT=`test -r "$CACHE_FILE" && cat "$CACHE_FILE" | awk '!seen[$0]++' | tail -n 10`

LOGINS=""
SELECTED="on"
for l in $DEFAULT; do
	LOGINS="$LOGINS $l $SELECTED"
	SELECTED="off"
done

CANCELLED=0
dialog --no-items --radiolist "rdesktop connection:" 50 70 45 $LOGINS "Add a new entry" off 2> "$TEMPFILE" || CANCELLED=1
INPUT=`cat "$TEMPFILE"`

if [ $CANCELLED -ne 0 ]; then
	exit 1
fi

if [ "$INPUT" = "Add a new entry" ]; then
	CANCELLED=0
	dialog --inputbox "New rdesktop connection:\n[user@]hostname[:port]?" 20 40 2> "$TEMPFILE" || CANCELLED=1
	INPUT=`cat "$TEMPFILE"`
	if [ $CANCELLED -ne 0 ]; then
		rm -f "$TEMPFILE"
		exit 1
	fi
fi

USER_OPT=`echo "$INPUT" | sed 's/^[^@]*$//; s/\(.*\)@.*/\1/; /^$/q ; s/^/-u /'`
HOSTNAME=`echo "$INPUT" | sed 's/^.*@//'`

if [ -z "$HOSTNAME" ]; then
	exit 1
fi

mkdir -p "$CACHE_DIR" && ( echo "$INPUT" > "$CACHE_FILE" ;
		for l in $DEFAULT; do
			if [ "$l" != "$INPUT" ]; then
				echo "$l" >> "$CACHE_FILE"
			fi
		done )

echo "rdesktop $USER_OPT -g 1600x960 -K $HOSTNAME" > "$TEMPFILE"
exit 0
