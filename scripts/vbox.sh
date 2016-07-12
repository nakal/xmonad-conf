#!/bin/sh

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
SELECTED="TRUE"
for l in $DEFAULT; do
	VMS="$VMS $SELECTED $l"
	SELECTED="FALSE"
done

INPUT=`zenity --list --radiolist --text "Select VBox VM:" --column "" --column "VM" $VMS`
if [ $? -ne 0 ]; then
	exit 0
fi

mkdir -p "$CACHE_DIR" && ( echo "$INPUT" > "$CACHE_FILE" ;
		for l in $DEFAULT; do
			if [ "$l" != "$INPUT" ]; then
				echo "$l" >> "$CACHE_FILE"
			fi
		done )

exec VBoxSDL --startvm "$INPUT" --nograbonclick
