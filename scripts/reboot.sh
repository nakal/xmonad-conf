#!/bin/sh

OS=`uname -s`

case "$OS" in
"OpenBSD") doas /sbin/shutdown -r now || /sbin/shutdown -r now ;;
*) sudo /sbin/shutdown -r now || /sbin/shutdown -r now ;;
esac
