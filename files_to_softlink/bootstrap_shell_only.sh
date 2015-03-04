#!/bin/sh

# Git repo helper
git_init_dir()
{
	REPOPATH="$1"
	REPODIR="$2"
	test -d "$REPODIR" && git pull
	test -d "$REPODIR" || git clone "$REPOPATH" "$REPODIR"
}

echo Checking packages...
pkg info vim git tmux fish \
	> /dev/null
if [ $? -ne 0 ]; then
	echo "ERROR: Missing packages for bootstrap (shell only)."
	exit 1
fi

cd $HOME
REMOVE_FILES=".cshrc .tmux.conf .indent.pro \
	.gitignore_global .gitconfig \
	$HOME/.config/fish/config.fish $HOME/.config/fish/custom \
	$HOME/.config/fish/functions \
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
echo Moving vim configuration out of the way...
mv .vim .vim-bak-`date +%s` 2> /dev/null
mv .vimrc .vimrc-bak-`date +%s` 2> /dev/null

# prepare conf in user's home
echo Reinstalling softlinks...
ln -s ~/.xmonad/files_to_softlink/shell/tcsh/.cshrc .
ln -s ~/.xmonad/files_to_softlink/tmux/.tmux.conf .
ln -s ~/.xmonad/files_to_softlink/misc/.indent.pro .
ln -s ~/.xmonad/files_to_softlink/git/.gitignore_global .
ln -s ~/.xmonad/files_to_softlink/git/.gitconfig .

echo Checking local tmux configuration...
if [ -r "$HOME/.tmux.local" ]; then
	echo "Already exists (skipping)."
else
	grep "set -g status-style " "$HOME/.tmux.conf" | sed 's/^/#/' > "$HOME/.tmux.local"
	echo "-> Installed. You can change tmux status bar color there."
fi

echo Preparing fish shell...
mkdir -p $HOME/.config/fish
cd $HOME/.config/fish
ln -s ~/.xmonad/files_to_softlink/shell/fish/config.fish .
ln -s ~/.xmonad/files_to_softlink/shell/fish/custom .
ln -s ~/.xmonad/files_to_softlink/shell/fish/functions .
cd $HOME
git_init_dir https://github.com/bpinto/oh-my-fish.git .oh-my-fish

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

exit 0
