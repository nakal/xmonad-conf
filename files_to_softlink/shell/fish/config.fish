
# Oh my fish
set fish_path $HOME/.oh-my-fish
set fish_theme robbyrussell
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

# Prompt
function prompt_pwd --description 'Print the current working directory, shortened to fit the prompt'
	echo $PWD | sed -e "s|^$HOME|~|"
end

function fish_prompt
	if not set -q __fish_prompt_hostname
		set -g __fish_prompt_hostname (hostname -s)
	end

	if not set -q __fish_prompt_normal
		set -g __fish_prompt_normal (set_color normal)
	end

	if not set -q __fish_prompt_prefix_color
		set -g __fish_prompt_prefix_color (set_color -o yellow)
	end

	if not set -q __fish_prompt_cwd
		set -g __fish_prompt_cwd (set_color $fish_color_cwd)
	end

	echo -n -s "$__fish_prompt_prefix_color" "$USER" @ "$__fish_prompt_hostname" "$__fish_prompt_normal" ' ' "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_normal" ' > '
end

set __git_status_slow_repo_size "500000"

function get_git_status -d "Git status on the right side"
	if command git rev-parse --is-inside-work-tree >/dev/null 2>&1

		set -l curgitpath (command realpath (git rev-parse --git-dir))
		set -l ref (command git symbolic-ref HEAD | sed  "s-refs/heads/--" | sed -e 's/^ *//' -e 's/ *$//' 2> /dev/null)

		if [ "$__git_status_last_git_path" != "$curgitpath" ]
			set -l reposize (command du -sk "$curgitpath" | sed 's/[^0-9].*//')
			if test "$reposize" -lt "$__git_status_slow_repo_size"
				set -l dirty (command git status -s -uno --ignore-submodules=dirty | wc -l | sed -e 's/^ *//' -e 's/ *$//' 2> /dev/null)
				set -l uncommitted (command git status -s --ignore-submodules=dirty | egrep "^\?\? " | wc -l | sed -e 's/^ *//' -e 's/ *$//' 2> /dev/null)
				set -l unpushed (command git status -sb --ignore-submodules=dirty | head -1 | grep '\[ahead ' | sed 's/.*\[ahead \([0-9][0-9]*\)\]$/\1/' 2> /dev/null)
				if [ "$dirty$uncommitted" != "00" ]
					set_color -b normal
					if [ "$dirty" != "0" ]
						set_color 822
						echo "$dirty ($uncommitted) "
					else
						set_color 666
						echo "($uncommitted) "
					end
				end

				if [ "$dirty" != "0" ]
					set_color -b 822
					set_color white
				else
					if [ "$unpushed" != "" ]
						set_color 282
						echo "-$unpushed "
						set_color -b 282
					else
						set_color -b 666
					end
					set_color white
				end
			else
				set -g __git_status_last_git_path "$curgitpath"
			end
		end

		if [ "$__git_status_last_git_path" = "$curgitpath" ]
			set_color -b normal
			set_color 288
			echo "(skipped) "
			set_color -b 288
			set_color white
		end
		echo "$ref"
		set_color normal
	end
end

function fish_right_prompt -d "Prints right prompt"
get_git_status
end
