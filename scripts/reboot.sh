#!/bin/sh

zenity --question --text "Reboot?" || exit 0

exec sudo /sbin/shutdown -r now
