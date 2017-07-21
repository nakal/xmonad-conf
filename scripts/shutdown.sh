#!/bin/sh

OS=`uname -s`

case "$OS" in
"FreeBSD") sudo /sbin/shutdown -p now || /sbin/shutdown -p now ;;
"OpenBSD") doas /sbin/shutdown -p now || /sbin/shutdown -p now ;;
*) sudo /sbin/shutdown -h now || /sbin/shutdown -h now ;;
esac
