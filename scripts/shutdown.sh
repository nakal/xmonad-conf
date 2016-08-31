#!/bin/sh

MYDIR=`dirname $0`
. $MYDIR/include.sh

if [ -z "$@" ]; then
	exec "$TERMINAL" "$CLASS_OPT" "xmDialog" -e $0 x
fi

dialog --yesno "Shutdown?" 5 40 || exit 0

OS=`uname -s`

case "$OS" in
"FreeBSD") exec sudo /sbin/shutdown -p now ;;
"OpenBSD") exec doas /sbin/shutdown -p now ;;
*) exec sudo /sbin/shutdown -h now ;;
esac
