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
