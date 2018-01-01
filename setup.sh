#!/bin/sh

OS=`uname -s`

checkfont() {
	FNTS=`fc-list "$1"`
	if [ -z "$FNTS" ]; then
		echo "WARNING: $1 is not available."
	fi
}

check_haskell() {
	ulimit_d=`ulimit -d`
	if [ "$ulimit_d" != "unlimited" ] && [ $ulimit_d -lt 1572864 ]; then
		echo "*** WARNING: please increase data seg size limit (ulimit -d)"
	fi
	echo "Checking Haskell packages..."
	if ! which cabal > /dev/null 2>&1; then
		echo "*** ERROR: cabal is not available"
		exit 1
	fi
	echo "Ok, found cabal, updating package list, please wait..."
	if ! cabal update; then
		echo "*** ERROR: cabal update failed."
		exit 1
	fi

	TMPFILE=`mktemp -t nakal_xmonad_setup.XXXXXX`
	cabal list --installed --simple-output > "$TMPFILE" 2>/dev/null
	HASKELL_PACKAGES="\
		xmonad xmonad-contrib network \
		"
	for pkg in $HASKELL_PACKAGES; do
		if ! egrep -q "^$pkg" "$TMPFILE"; then
			echo "Haskell installation missing $pkg, installing..."
			if ! cabal install "$pkg"; then
				echo "*** ERROR (installation failed): cabal" \
					" install $pkg"
				exit 1
			fi
		else
			echo "-> $pkg is installed"
		fi
	done

	# cabal does not register Xmobar
	if which xmobar; then
		cabal_install_options="--flags=with_xft"
		case $OS in
			OpenBSD) cabal_install_options="$cabal_install_options --extra-lib-dirs=/usr/X11R6/lib --extra-include-dirs=/usr/X11R6/include" ;;
			FreeBSD) cabal_install_options="$cabal_install_options --with-gcc /usr/local/bin/gcc6" ;;
			*) ;;
		esac
		if ! cabal install -v xmobar $cabal_install_options; then
			echo "*** ERROR (installation failed): cabal" \
				" install $pkg $cabal_install_options"
			exit 1
		fi
	fi

	rm "$TMPFILE"
}

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

echo "[Xmonad setup] Updating shell-setup first..."

# Execute it first
cd "$SCRIPT_HOME"
SHELLSETUP="./shell-setup/setup.sh"
if [ ! -x "$SHELLSETUP" ]; then
	echo "[Xmonad setup] Don't have submodule, yet. Checking out..."
	git submodule init
	git submodule update
	if [ $? -ne 0 ]; then
		echo "*** FAILED to check out."
		exit 1
	fi
else
	echo "[Xmonad setup] Have submodule. Updating..."
	cd ./shell-setup
	if git symbolic-ref --short HEAD; then
		if ! git pull; then
			echo "*** FAILED to pull master"
			exit 1
		fi
	else
		if ! git submodule update; then
			echo "*** FAILED to update submodule"
			exit 1
		fi
	fi
	cd "$SCRIPT_HOME"
fi

echo "[Xmonad setup] Checking fonts..."
checkfont "Fantasque Sans Mono"

REQUIRED_PACKAGES_OpenBSD="\
	cabal-install ghc cargo dmenu gmrun xscreensaver dialog \
	gnupg-2 gpgme rxvt-unicode xdotool xclip \
	"
RECOMMENDED_PACKAGES_OpenBSD="\
	firefox gimp weechat xfe mozilla-dicts-de-DE password-gorilla \
"
REQUIRED_PACKAGES_FreeBSD="\
	sudo rxvt-unicode xscreensaver \
	dmenu gmrun xrdb xsetroot setxkbmap gnupg \
	xmodmap hsetroot fira hs-cabal-install ghc \
	roboto-fonts-ttf xdotool xclip xwininfo \
	gcc6"
RECOMMENDED_PACKAGES_FreeBSD="\
	xdm gimp weechat firefox xfe gtk2 gtk-oxygen-engine gorilla \
	"
. "$SCRIPT_HOME/shell-setup/include/packages.sh"
check_packages

check_haskell

echo "[Xmonad setup] Executing shell-setup..."
$SHELLSETUP
if [ $? -ne 0 ]; then
	echo "Submodule shell-setup failed. Aborting."
	exit 1
fi

cd $HOME
REMOVE_FILES=".xinitrc .xsession .Xdefaults .gtkrc-2.0 \
	.config/gtk-3.0/settings.ini .config/user-dirs.dirs \
	.vimperatorrc.local .xpdfrc_base \
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
ln -s $SCRIPT_HOME/xsettings/.xinitrc .xsession
ln -s $SCRIPT_HOME/xsettings/.Xdefaults .
ln -s $SCRIPT_HOME/xsettings/.gtkrc-2.0 .
ln -s $SCRIPT_HOME/xsettings/.vimperatorrc.local .
ln -s $SCRIPT_HOME/xsettings/.xpdfrc_base .

mkdir -p $HOME/.config
cd $HOME/.config
ln -s $SCRIPT_HOME/xsettings/user-dirs.dirs .

mkdir -p $HOME/.config/gtk-3.0
cd $HOME/.config/gtk-3.0
ln -s $SCRIPT_HOME/xsettings/settings.ini .

# xpdf local configuration
if [ ! -e $HOME/.xpdfrc ]; then
	echo "include			\"$HOME/.xpdfrc_base\"" > $HOME/.xpdfrc
	echo "psFile                  \"|/usr/local/bin/lpr\"" >> $HOME/.xpdfrc
	echo "WARNING: A default ~/.xpdfrc file has been written."
	echo "         You might want to append ' -P printer' to the print command there."
fi

echo "[Xmonad setup] Checking software capabilities..."
echo "Checking xmonad..."
xmonad --version | egrep -q "xmonad 0.13"
if [ $? -ne 0 ]; then
	echo "*** need Xmonad 0.13"
	exit 1
else
	echo "-> Xmonad is ok, good."
fi

cd $HOME
echo Preparing xmonad...
xmonad --recompile
if [ $? -ne 0 ]; then
	echo "*** Building xmonad failed"
	exit 1
fi

echo Preparing SysInfoBar...
if [ "$OS" = "FreeBSD" ]; then
	cd $HOME/.xmonad/lib
	ghc --make SysInfoBar.hs
	if [ $? -ne 0 ]; then
		echo "*** Building SysInfoBar binary failed"
		exit 1
	fi
else
	echo "Skipping build... searching for sysinfobar..."
	which sysinfobar > /dev/null 2>&1 || test -x $HOME/.cargo/bin/sysinfobar
	if [ $? -ne 0 ]; then
		echo "*** No sysinfobar installed"
		exit 1
	else
		echo "-> sysinfobar found."
	fi
fi

echo "-----------------------------------------------------------------"
echo "[Xmonad setup] Finished successfully."
echo "-----------------------------------------------------------------"
echo "Don't forget to copy $SCRIPT_HOME/xkb/us_alt"
echo "for keyboard bindings."
echo "Also restart xmonad with:"
echo "xmonad --restart"
echo "-----------------------------------------------------------------"

exit 0
