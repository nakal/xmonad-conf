#!/bin/sh

MODE=$1
DPI=$2

if [ -n $MODE ]; then
	MODE=2560x1440
fi

if [ -n $DPI ]; then
	DPI=166
fi

# Try resolution
xrandr --dpi $DPI --output VGA-0 --mode $MODE 2> /dev/null
if [ $? -eq 0 ]; then
	echo "Xtf.dpi: $DPI" | xrdb -m
fi

exit 0
