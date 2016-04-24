#!/bin/sh

zenity --question --text "Reboot?" || exit 0

OS=`uname -s`

case "$OS" in
"OpenBSD") exec doas /sbin/shutdown -r now ;;
*) exec sudo /sbin/shutdown -r now ;;
esac
