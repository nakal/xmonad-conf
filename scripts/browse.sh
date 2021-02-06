#!/bin/sh

if [ -z "$BROWSER" ]; then
	BROWSER=chrome
	export BROWSER
fi

exec $BROWSER $@
