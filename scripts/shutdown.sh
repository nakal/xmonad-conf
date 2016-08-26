#!/bin/sh

if [ -z "$@" ]; then
	exec xterm -class "Dialog" -e $0 x
fi

dialog --yesno "Shutdown?" 5 40 || exit 0

OS=`uname -s`

case "$OS" in
"FreeBSD") exec sudo /sbin/shutdown -p now ;;
"OpenBSD") exec doas /sbin/shutdown -p now ;;
*) exec sudo /sbin/shutdown -h now ;;
esac
