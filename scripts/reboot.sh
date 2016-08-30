#!/bin/sh

if [ -z "$@" ]; then
	exec xterm -class "Dialog" -e $0 x
fi

dialog --yesno "Reboot?" 5 40 || exit 0

OS=`uname -s`

case "$OS" in
"OpenBSD") exec doas /sbin/shutdown -r now ;;
*) exec sudo /sbin/shutdown -r now ;;
esac
