export LANG="de_DE.UTF-8"
export LC_CTYPE="de_DE.UTF-8"

OS=`uname -s`
echo "Starting $0 on $OS ..." >&2
case "$OS" in
FreeBSD)
	xset fp+ /usr/local/share/fonts/fantasque-sans-mono,/usr/local/share/fonts/fira
	;;
OpenBSD)
	xset fp+ /usr/local/share/fonts/fantasque-sans,/usr/local/share/fonts/fira
	;;
*)
	;;
esac
xset fp rehash

ulimit -c unlimited

xrdb -merge ~/.Xdefaults
xsetroot -cursor_name left_ptr -solid black
setxkbmap -symbols "us_alt(de)"
xmodmap -e "pointer = 1 2 3 4 5 6 7 0 0"
xscreensaver -no-splash &

PATH=$HOME/.cabal/bin:$HOME/.cargo/bin:$HOME/bin:$PATH
export PATH
DEFAULT_SESSION=xmonad

if [ -n "$GPG_AGENT_INFO" ]; then
	SESSION_AGENT="ssh-agent"
else
	SESSION_AGENT="gpg-agent --daemon --default-cache-ttl 7200 ssh-agent"
	# Hack for mutt
	export GPG_AGENT_INFO="$HOME/.gnupg/S.gpg-agent:0:1"
fi

echo "START SESSION: $1" `date` >&2

case "$1" in
spectrwm)
	exec $SESSION_AGENT spectrwm >&2
	;;
openbox)
	exec $SESSION_AGENT openbox-session
	;;
fluxbox)
	exec $SESSION_AGENT startfluxbox
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
	_JAVA_AWT_WM_NONREPARENTING=1
	export _JAVA_AWT_WM_NONREPARENTING
	exec $SESSION_AGENT "$DEFAULT_SESSION" > ~/.xsession-errors 2>&1
	;;
esac
