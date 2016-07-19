export LANG="de_DE.UTF-8"
export TERM="screen-256color"
ulimit -c unlimited
#export LC_ALL="de_DE.UTF-8"

xset +fp /usr/local/share/fonts/fantasque-sans-mono,/usr/local/share/fonts/fira,/usr/local/share/fonts/FontAwesome
xrdb -merge ~/.Xdefaults
xsetroot -cursor_name left_ptr -solid black
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

# Hack for mutt
export GPG_AGENT_INFO="$HOME/.gnupg/S.gpg-agent:0:1"

echo "START SESSION: $1" `date` >> ~/.xsession-errors

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
