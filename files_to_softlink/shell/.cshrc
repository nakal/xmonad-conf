# $FreeBSD$
#
# .cshrc - csh resource script, read at beginning of execution by each shell
#
# see also csh(1), environ(7).

alias h		history 25
alias j		jobs -l
alias ls	ls -G
alias la	ls -aF
alias lf	ls -FA
alias ll	ls -lAF
alias l		ll
alias ..	cd ..
alias ...	cd ../..

alias vi	vim

# A righteous umask
umask 22

set path = (/sbin /bin /usr/sbin /usr/bin /usr/games /usr/local/sbin /usr/local/bin $HOME/bin)
limit coredumpsize unlimited

setenv	EDITOR	vim
setenv	PAGER	less
setenv	BLOCKSIZE	K
setenv LSCOLORS gxfxcxdxbxegedabagacad
setenv GOROOT /usr/local/go

if ($?prompt) then

	if ($USER == "root") then
		# Red prompt
		set prompt = "%B%{\033[1;31m%}%N@%m%b %~ %#%{\033[0m%} "
	else
		# Yellow prompt
		set prompt = "%B%{\033[1;33m%}%N@%m%b %~ %#%{\033[0m%} "
	endif
	set promptchars = "%#"

	set filec
	set history = 1000
	set savehist = (1000 merge)
	set autolist = ambiguous
	# Use history to aid expansion
	set autoexpand
	set autorehash
	set nobeep
	set rmstar
	set mail = (/var/mail/$USER)
	if ( $?tcsh ) then
		bindkey "^W" backward-delete-word
		bindkey -k up history-search-backward
		bindkey -k down history-search-forward
	endif

endif

set HOSTHASH=`echo -n $HOST:r:e$USER | sha256`
if ($HOSTHASH == "9cc9bab29b2db5337f4983ba2438b2667a6fac5a6f26baa8522995b607460647") then
	set fignore = source
	setenv ANDROID_SDK_HOME /home/${USER}/src/android-sdk-linux
endif

unset HOSTHASH
