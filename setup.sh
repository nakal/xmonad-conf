#!/bin/sh

echo "[Xmonad setup] Looking for my installation directory..."
SCRIPT_HOME=`dirname $0`
SCRIPT_HOME=`readlink -f "$SCRIPT_HOME"`
XMONAD_HOME=`readlink -f "$HOME/.xmonad"`
if [ "$SCRIPT_HOME" != "$XMONAD_HOME" ]; then
	echo "The Xmonad project should be placed directly in\
 your $XMONAD_HOME directory."
	echo "Aborting."
	exit 1
fi

if [ -x "$SCRIPT_HOME/setup.sh" ]; then
	echo "Found myself in $SCRIPT_HOME, good."
else
	echo "Hmm, I cannot find my own directory. Giving up..."
	exit 1
fi

# Execute it first
cd "$SCRIPT_HOME"
SHELLSETUP="./shell-setup/setup.sh"
if [ ! -x "$SHELLSETUP" ]; then
	git submodule init
	git submodule update
else
	cd ./shell-setup
	git pull
	cd "$SCRIPT_HOME"
fi
$SHELLSETUP || (echo "Submodule shell-setup failed. Aborting." && exit 1)

OS=`uname -s`
if [ "$OS" = "FreeBSD" ]; then
	echo "[Xmonad setup] Checking packages..."
	pkg info slim sudo gtk2 rxvt-unicode xscreensaver \
		hs-xmonad hs-network hs-xmonad-contrib hs-xmobar \
		firefox dmenu gmrun weechat zenity claws-mail \
		gtk-oxygen-engine xrdb xsetroot setxkbmap gnupg \
		xmodmap hsetroot inconsolata-ttf fira fantasque-sans-mono \
		xdotool \
		> /dev/null
	if [ $? -ne 0 ]; then
		echo "ERROR: Missing packages for setup (for X)."
		exit 1
	fi
	echo "[Xmonad setup] Checking recommended packages..."
	pkg info gimp libreoffice weechat > /dev/null
	if [ $? -ne 0 ]; then
		echo "WARNING: Some recommended packages are not installed."
	fi
else
	echo "[Xmonad setup] WARNING: Skipped checking packages..."
fi

echo "[Xmonad setup] Checking software capabilities..."
echo "Checking xmonad..."
xmonad --version | egrep -q "xmonad 0.11"
if [ $? -ne 0 ]; then
	echo "*** need Xmonad 0.11."
	exit 1
else
	echo "-> Xmonad is ok, good."
fi

# dzen2 is still needed for mail notifications
echo "Checking dzen2..."
if [ "x$DISPLAY" = "x" ]; then
	echo "??? skipping dzen2, no X server running."
	echo "??? assuming dzen2 works fine."
else
	dzen2 -v | grep -q XFT
	if [ $? -ne 0 ]; then
		echo "*** dzen2 appears to be too old and does not support XFT."
		exit 1
	else
		echo "-> XFT found, good."
	fi
fi

cd $HOME
REMOVE_FILES=".xinitrc .Xdefaults .gtkrc-2.0 \
	.config/gtk-3.0/settings.ini .config/user-dirs.dirs \
	"

for df in $REMOVE_FILES; do
	echo Checking dotfile: $df
	test -e $df && test ! -L $df && \
		echo "*** It is not a soft-link. Please move it to a safe location!" && exit 1
done

# remove old stuff
cd $HOME
echo "[Xmonad setup] Removing old softlinks..."
rm -f $REMOVE_FILES

# prepare conf in user's home
echo "[Xmonad setup] Reinstalling softlinks..."
ln -s $SCRIPT_HOME/xsettings/.xinitrc .
ln -s $SCRIPT_HOME/xsettings/.Xdefaults .
ln -s $SCRIPT_HOME/xsettings/.gtkrc-2.0 .

mkdir -p $HOME/.config
cd $HOME/.config
ln -s $SCRIPT_HOME/xsettings/user-dirs.dirs .

mkdir -p $HOME/.config/gtk-3.0
cd $HOME/.config/gtk-3.0
ln -s $SCRIPT_HOME/xsettings/settings.ini .

cd $HOME
echo Preparing xmonad...
xmonad --recompile

echo "-----------------------------------------------------------------"
echo "[Xmonad setup] Finished successfully."
echo "-----------------------------------------------------------------"
echo "Don't forget to copy $SCRIPT_HOME/xkb/us_alt"
echo "for keyboard bindings."
echo "Also restart xmonad with:"
echo "xmonad --restart"
echo "-----------------------------------------------------------------"

exit 0
