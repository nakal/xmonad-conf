#!/bin/sh

if [ -z "$@" ]; then
	exec xterm -class "xmDialog" -e $0 x
fi

CACHE_DIR="$HOME/.cache/xmonad-conf"
CACHE_FILE="$CACHE_DIR/vbox-last.txt"

VMLISTFILE=`mktemp -t "vboxlist"`
VBoxManage list vms | sed 's/^"\([^"]*\)".*/\1/' > "$VMLISTFILE"

VMLASTFILE=`mktemp -t "vboxlast"`
test -r "$CACHE_FILE" && cat "$CACHE_FILE" | awk '!seen[$0]++' > "$VMLASTFILE"

DEFAULT=$(grep -xf "$VMLISTFILE" "$VMLASTFILE" | tail -n 10)
REMAIN=$(grep -xvf "$VMLASTFILE" "$VMLISTFILE")
DEFAULT="$DEFAULT $REMAIN"

rm -f "$VMLISTFILE" "$VMLASTFILE"

VMS=""
SELECTED="on"
for l in $DEFAULT; do
	VMS="$VMS $l $SELECTED"
	SELECTED="off"
done

TEMPFILE=`mktemp -t xmonad-script-output`
dialog --no-items --radiolist "Select VBox VM:" 50 70 45 $VMS 2> "$TEMPFILE" || exit 0
INPUT=`cat "$TEMPFILE"`
rm -f "$TEMPFILE"

mkdir -p "$CACHE_DIR" && ( echo "$INPUT" > "$CACHE_FILE" ;
		for l in $DEFAULT; do
			if [ "$l" != "$INPUT" ]; then
				echo "$l" >> "$CACHE_FILE"
			fi
		done )

nohup VBoxSDL --startvm "$INPUT" --nograbonclick > /dev/null 2>&1 &

exit 0
