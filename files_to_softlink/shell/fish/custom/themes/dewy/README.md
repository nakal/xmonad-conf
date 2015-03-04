# dewy

dewy is a simple kind of prompt with a bit more efficient Git support on the
right side. It has been inspired by `cmorrell.com`, but altered to avoid
some annoyances.

![dewy][screenshot]

## Features

* Showing `user@host` to easily see where you are logged in.
* Showing full path (only $HOME is abbreviated), instead of shortened version
  and surrounded by blanks to support copy and paste of the current directory.
* Git integration
	* Shows gray branch name, if clean.
	* Shows number in parentheses untracked files.
	* Shows number of files changed in red.
	* Shows -number in green for branch that is ahead.
	* Shows branch in cyan without status info, if the repository would
		slow down the prompt too much.
	* Shows pink branch name while rebase is in progress

[screenshot]: https://lh3.googleusercontent.com/-RCKrxVbW7uk/VPD97XVYhLI/AAAAAAAAGNo/JmP7KsQNc4o/w732-h84-no/fish_dewy_theme.png
