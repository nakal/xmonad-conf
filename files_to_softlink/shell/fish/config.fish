
# Oh my fish
set fish_path $HOME/.oh-my-fish
set fish_theme dewy
set fish_plugins theme
#set fish_custom $HOME/.xmonad/files_to_softlink/shell/fish/custom
set fish_custom $HOME/.config/fish/custom
. $fish_path/oh-my-fish.fish

# Aliases
function l
	ls -l $argv
end

function ...
	cd ../..
end

function vi
	vim $argv
end

# Environment
set -gx PATH $PATH /usr/games /sbin /usr/sbin /usr/local/sbin /usr/local/bin $HOME/bin
set -gx EDITOR vim
set -gx PAGER less
set -gx BLOCKSIZE K
set -gx LSCOLORS gxfxcxdxbxegedabagacad
set -gx TERM screen-256color
set -gx GOROOT /usr/local/go

# Settings
ulimit -c unlimited
