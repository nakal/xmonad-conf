#!/bin/sh

echo Checking packages...
pkg info slim sudo gtk2 vim git xterm xscreensaver \
	hs-xmonad hs-network hs-xmonad-contrib \
	firefox gimp libreoffice dmenu gmrun tmux \
	dzen2 weechat-devel zenity claws-mail \
	gtk-oxygen-engine xrdb xsetroot setxkbmap gnupg \
	xmodmap hsetroot \
	> /dev/null
if [ $? -ne 0 ]; then
	echo ERROR: Missing packages for bootstrap.
	exit 1
fi

cd $HOME
REMOVE_FILES=".cshrc .xinitrc .Xdefaults .gtkrc-2.0 .tmux.conf .indent.pro \
	~/.config/gtk-3.0/settings.ini .gitignore_global .gitconfig \
	~/.config/fish/config.fish"

for df in $REMOVE_FILES; do
	echo Checking dotfile: $df
	test -e $df && test ! -L $df && \
		echo "*** It is not a soft-link. Please move it to a safe location!" && exit 1
done

# remove old stuff
cd $HOME
echo Removing old softlinks...
rm -f $REMOVE_FILES
echo Moving vim configuration out of the way...
mv .vim .vim-bak-`date +%s` 2> /dev/null
mv .vimrc .vimrc-bak-`date +%s` 2> /dev/null

# prepare conf in user's home
echo Reinstalling softlinks...
ln -s ~/.xmonad/files_to_softlink/shell/.cshrc .
ln -s ~/.xmonad/files_to_softlink/xsettings/.xinitrc .
ln -s ~/.xmonad/files_to_softlink/xsettings/.Xdefaults .
ln -s ~/.xmonad/files_to_softlink/xsettings/.gtkrc-2.0 .
ln -s ~/.xmonad/files_to_softlink/tmux/.tmux.conf .
ln -s ~/.xmonad/files_to_softlink/misc/.indent.pro .
ln -s ~/.xmonad/files_to_softlink/git/.gitignore_global .
ln -s ~/.xmonad/files_to_softlink/git/.gitconfig .

mkdir -p $HOME/.config/gtk-3.0
cd $HOME/.config/gtk-3.0
ln -s ~/.xmonad/files_to_softlink/xsettings/settings.ini .

mkdir -p $HOME/.config/fish
cd $HOME/.config/fish
ln -s ~/.xmonad/files_to_softlink/shell/config.fish .

echo Initial git setup...
cd $HOME
git config --global core.excludesfile ~/.gitignore_global

echo Preparing vim and plugins...
mkdir -p .vim/bundle .vim/autoload .vim/colors
cd .vim
ln -s ~/.xmonad/files_to_softlink/vim/vimrc .
ln -s ~/.xmonad/files_to_softlink/vim/update-plugins.sh .
cd autoload
git clone https://github.com/tpope/vim-pathogen.git
ln -s vim-pathogen/autoload/pathogen.vim .
cd ../bundle
git clone https://github.com/Shougo/unite.vim
git clone https://github.com/tsukkee/unite-tag
git clone https://github.com/tpope/vim-fugitive
git clone https://github.com/tpope/vim-unimpaired
git clone https://github.com/altercation/vim-colors-solarized
git clone https://github.com/bling/vim-airline
cd ../colors
git clone https://github.com/gosukiwi/vim-atom-dark.git
ln -s vim-atom-dark/colors/atom-dark.vim .
ln -s vim-atom-dark/colors/atom-dark-256.vim .
cd ../..

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
