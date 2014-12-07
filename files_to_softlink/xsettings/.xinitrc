export LANG="de_DE.UTF-8"
ulimit -c unlimited
#export LC_ALL="de_DE.UTF-8"

xrdb -merge ~/.Xdefaults
xsetroot -cursor_name left_ptr
setxkbmap -symbols "us_alt(de)"
xmodmap -e "pointer = 1 2 3 4 5 6 7 0 0"
xscreensaver -no-splash &

#CUSTOM_SESSION=$HOME/.spectrwm/src/spectrwm/linux/spectrwm
#if [ -x "$CUSTOM_SESSION" ]; then
#	DEFAULT_SESSION=$CUSTOM_SESSION
#else
#	DEFAULT_SESSION=spectrwm
#fi
#GPG_AGENT="gpg-agent --daemon --enable-ssh-support"
DEFAULT_SESSION=xmonad
GPG_AGENT="gpg-agent --daemon"

case $1 in
spectrwm)
	exec $GPG_AGENT spectrwm >> ~/.xsession-errors 2>&1
	;;
xmonad)
	exec $GPG_AGENT "$DEFAULT_SESSION" >> ~/.xsession-errors 2>&1
	;;
openbox)
	exec $GPG_AGENT openbox-session
	;;
fluxbox)
	exec $GPG_AGENT startfluxbox
	;;
gnome)
	exec gnome-session
	;;
kde)
	PATH=$PATH:/usr/local/kde4/bin
	export PATH
	exec /usr/local/kde4/bin/kdm
	;;
*)
	exec $GPG_AGENT "$DEFAULT_SESSION" >> ~/.xsession-errors 2>&1
	;;
esac
