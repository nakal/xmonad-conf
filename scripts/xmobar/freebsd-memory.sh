#!/bin/sh

MEMTOTAL=`/sbin/sysctl -n vm.stats.vm.v_page_count`
MEMFREE=`/sbin/sysctl -n vm.stats.vm.v_free_count`
MEMINACT=`/sbin/sysctl -n vm.stats.vm.v_inactive_count`

MEMPERC=`expr \( $MEMTOTAL - $MEMFREE + $MEMINACT \) \* 100 / $MEMTOTAL`

if [ $MEMPERC -le 33 ]; then
	echo "$MEMPERC%"
else
	if [ $MEMPERC -lt 50 ]; then
		echo "<fc=#c08000>$MEMPERC%</fc>"
	else
		echo "<fc=#c04040>$MEMPERC%</fc>"
	fi
fi

exit 0
