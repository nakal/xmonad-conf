#!/bin/sh

SWAPINFO=`/usr/sbin/pstat -T | tail -n 1 | sed 's/[^0-9\/]//g'`

SWAPUSED=`echo $SWAPINFO | sed s+/.*$++`
SWAPTOTAL=`echo $SWAPINFO | sed s+^[^/]*/++`

SWAPPERC=`expr 100 \* $SWAPUSED / $SWAPTOTAL`

if [ $SWAPPERC -lt 20 ]; then
	echo "$SWAPPERC%"
else
	if [ $SWAPPERC -lt 50 ]; then
		echo "<fc=#c08000>$SWAPPERC%</fc>"
	else
		echo "<fc=#c04040>$SWAPPERC%</fc>"
	fi
fi

exit 0
