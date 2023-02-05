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
	if ! cabal new-update; then
		echo "*** ERROR: cabal new-update failed."
		exit 1
	fi

	HASKELL_PACKAGES="\
		unordered-containers X11 data-default \
		vector \
		"

	if [ "$OS" != "Linux" ]; then
		HASKELL_PACKAGES="$HASKELL_PACKAGES \
		xmonad xmonad-contrib hostname aeson \
		"
	fi


	if [ "$OS" = "FreeBSD" ]; then
		HASKELL_PACKAGES="$HASKELL_PACKAGES bsd-sysctl"
	fi

	for pkg in $HASKELL_PACKAGES; do
		if ! ghc-pkg describe "$pkg" > /dev/null; then
			echo "Haskell installation missing $pkg, installing..."
			if [ "$pkg" = "xmonad" ]; then
				libflag=""
			else
				libflag="--lib"
			fi
			if ! cabal new-install $libflag --overwrite-policy=always "$pkg"; then
				echo "*** ERROR (installation failed): cabal" \
					" new-install $pkg"
				exit 1
			fi
		else
			echo "-> $pkg is installed"
		fi
	done

	# cabal does not register Xmobar
	if ! which xmobar; then
		cabal_install_options="-fwith_utf8 -fwith_xft"
		case $OS in
			OpenBSD) cabal_install_options="$cabal_install_options --extra-lib-dirs=/usr/X11R6/lib --extra-include-dirs=/usr/X11R6/include" ;;
			*) ;;
		esac
		if ! cabal new-install -v xmobar $cabal_install_options; then
			echo "*** ERROR (installation failed): cabal" \
				" new-install $pkg $cabal_install_options"
			exit 1
		fi
	fi

	if [ "$OS" != "Linux" ]; then
		echo "Fixing ghc package list manually..."
		ghc-pkg recache --user
		ghc_version=`ghc --numeric-version`
		if ! cp $HOME/.cabal/store/ghc-$ghc_version/package.db/package.cache* $HOME/.ghc/*-*-$ghc_version/package.conf.d/; then
			echo "*** ERROR: Could not fix GHC package list."
			exit 1
		fi
	fi
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
	firefox gimp xfe mozilla-dicts-de-DE password-gorilla \
"
REQUIRED_PACKAGES_FreeBSD="\
	sudo rxvt-unicode xscreensaver \
	dmenu gmrun xrdb xsetroot setxkbmap gnupg \
	xmodmap hsetroot fira hs-cabal-install ghc \
	roboto-fonts-ttf xdotool xclip xwininfo \
	pkgconf xterm \
	"
RECOMMENDED_PACKAGES_FreeBSD="\
	xdm gimp xfe gtk2 gtk-oxygen-engine gorilla \
	"

REQUIRED_PACKAGES_Linux="\
	libpango1.0-dev libcairo2-dev rxvt-unicode \
	libghc-glib-dev libxft-dev libxss-dev fonts-fantasque-sans \
	libxrandr-dev lynx gnupg2 exuberant-ctags tmux \
	libx11-dev xmonad xmobar libghc-xmonad-dev libghc-xmonad-contrib-dev \
	libghc-aeson-dev libghc-hostname-dev libghc-data-default-dev \
	suckless-tools \
	"
RECOMMENDED_PACKAGES_Linux="\
	password-gorilla xpdf \
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
	.config/gtk-3.0/settings.ini \
	.vimperatorrc.local .xpdfrc_base .office.sh \
	.config/alacritty/alacritty.yml \
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
ln -s $SCRIPT_HOME/scripts/office.sh .office.sh

mkdir -p $HOME/.config
cd $HOME/.config

mkdir -p $HOME/.config/gtk-3.0
cd $HOME/.config/gtk-3.0
ln -s $SCRIPT_HOME/xsettings/settings.ini .

mkdir -p $HOME/.config/alacritty
cd $HOME/.config/alacritty
ln -s $SCRIPT_HOME/xsettings/alacritty.yml .

# xpdf local configuration
if [ ! -e $HOME/.xpdfrc ]; then
	echo "include			\"$HOME/.xpdfrc_base\"" > $HOME/.xpdfrc
	echo "psFile                  \"|/usr/local/bin/lpr\"" >> $HOME/.xpdfrc
	echo "WARNING: A default ~/.xpdfrc file has been written."
	echo "         You might want to append ' -P printer' to the print command there."
fi

echo "[Xmonad setup] Checking software capabilities..."
echo "Checking xmonad..."
xmonad --version
if [ $? -ne 0 ]; then
	echo "*** need Xmonad"
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
