set __git_status_slow_repo_size "2500"

function get_git_status -d "Git status on the right side"

	set -l is_git_workdir (command git rev-parse --is-inside-work-tree 2> /dev/null)
	if [ "$is_git_workdir" = "true" ]

		set -l curgitpath (command realpath (git rev-parse --git-dir))
		set -l ref (command git symbolic-ref HEAD | sed  "s-refs/heads/--" | sed -e 's/^ *//' -e 's/ *$//' 2> /dev/null)

		if [ "$__git_status_last_git_path" != "$curgitpath" ]
			set -l reposize (command du -sk "$curgitpath/index" 2> /dev/null | sed 's/[^0-9].*//')
			if test "$reposize" -lt "$__git_status_slow_repo_size"
				set -l dirty (command git status -s -uno --ignore-submodules=dirty | wc -l | sed -e 's/^ *//' -e 's/ *$//' 2> /dev/null)
				set -l uncommitted (command git status -s --ignore-submodules=dirty | egrep "^\?\? " | wc -l | sed -e 's/^ *//' -e 's/ *$//' 2> /dev/null)
				set -l unpushed (command git status -sb --ignore-submodules=dirty | head -1 | grep '\[ahead ' | sed 's/.*\[ahead \([0-9][0-9]*\).*/\1/' 2> /dev/null)
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
