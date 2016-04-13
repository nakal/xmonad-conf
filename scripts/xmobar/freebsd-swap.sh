#!/bin/sh

SWAPINFO=`/usr/sbin/pstat -T | tail -n 1 | sed 's/[^0-9\/]//g'`

SWAPUSED=`echo $SWAPINFO | sed s+/.*$++`
SWAPTOTAL=`echo $SWAPINFO | sed s+^[^/]*/++`

SWAPPERC=`expr 100 \* $SWAPUSED / $SWAPTOTAL`
#echo "XXX $SWAPUSED / $SWAPTOTAL / $SWAPINFO XXX"
#echo $PATH

if [ $SWAPPERC -le 33 ]; then
	echo "$SWAPPERC%"
else
	if [ $SWAPPERC -lt 80 ]; then
		echo "<fc=#c08000>$SWAPPERC%</fc>"
	else
		echo "<fc=#c04040>$SWAPPERC%</fc>"
	fi
fi

exit 0
