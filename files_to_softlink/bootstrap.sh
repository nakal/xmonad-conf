#!/bin/sh

echo Preparing shell-based access...
THISDIR=`dirname $0`
$THISDIR/bootstrap_shell_only.sh

echo Checking packages...
pkg info slim sudo gtk2 xterm xscreensaver \
	hs-xmonad hs-network hs-xmonad-contrib \
	firefox gimp libreoffice dmenu gmrun \
	dzen2 weechat-devel zenity claws-mail \
	gtk-oxygen-engine xrdb xsetroot setxkbmap gnupg \
	xmodmap hsetroot \
	> /dev/null
if [ $? -ne 0 ]; then
	echo "ERROR: Missing packages for bootstrap (for X)."
	exit 1
fi

cd $HOME
REMOVE_FILES=".xinitrc .Xdefaults .gtkrc-2.0 \
	$HOME/.config/gtk-3.0/settings.ini \
	"

for df in $REMOVE_FILES; do
	echo Checking dotfile: $df
	test -e $df && test ! -L $df && \
		echo "*** It is not a soft-link. Please move it to a safe location!" && exit 1
done

# remove old stuff
cd $HOME
echo Removing old softlinks...
rm -f $REMOVE_FILES

# prepare conf in user's home
echo Reinstalling softlinks...
ln -s ~/.xmonad/files_to_softlink/xsettings/.xinitrc .
ln -s ~/.xmonad/files_to_softlink/xsettings/.Xdefaults .
ln -s ~/.xmonad/files_to_softlink/xsettings/.gtkrc-2.0 .

mkdir -p $HOME/.config/gtk-3.0
cd $HOME/.config/gtk-3.0
ln -s ~/.xmonad/files_to_softlink/xsettings/settings.ini .

cd $HOME
echo Preparing xmonad...
xmonad --recompile

echo "-----------------------------------------------------------------"
echo "DONE!"
echo "-----------------------------------------------------------------"
echo "Don't forget to copy ~/.xmonad/files_to_softlink/xsettings/us_alt"
echo "for keyboard bindings."
echo "Also restart xmonad with:"
echo "xmonad --restart"
echo "-----------------------------------------------------------------"
exit 0
