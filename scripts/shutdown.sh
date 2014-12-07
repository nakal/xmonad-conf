#!/bin/sh

zenity --question --text "Shutdown?" || exit 0

OS=`uname -s`

case "$OS" in
"FreeBSD") exec sudo /sbin/shutdown -p now ;;
*) exec sudo /sbin/shutdown -h now ;;
esac
