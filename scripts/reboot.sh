#!/bin/sh

MYDIR=`dirname $0`
. $MYDIR/include.sh

if [ -z "$@" ]; then
	exec "$TERMINAL" "$CLASS_OPT" "xmDialog" -e $0 x
fi

dialog --yesno "Reboot?" 5 40 || exit 0

OS=`uname -s`

case "$OS" in
"OpenBSD") doas /sbin/shutdown -r now || /sbin/shutdown -r now ;;
*) sudo /sbin/shutdown -r now || /sbin/shutdown -r now ;;
esac
