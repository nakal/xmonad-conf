# nakal's Xmonad (plus desktop) configuration

![Desktop screenshot](https://lh5.googleusercontent.com/-GqhqGzydiw0/VIR8dN84OpI/AAAAAAAAGGY/Uce3Zy5FHaA/w862-h485-no/xmonad-desktop.png)

[Xmonad](http://xmonad.org/) is a minimalistic tiled window manager for Xorg.
To configure it, you need some [Haskell](http://haskel.org/) knowledge. It is
a good idea to learn some Haskell basics before using Xmonad. Sooner or later
you would like to make adaptations which are not solved by guessing the syntax.

Please be aware that this repository does not just host an Xmonad
configuration, but also some additional goodies for a usable desktop.

Included fine tuning has been done for (e.g.):

* csh
* vim
* tmux
* git
* indent
* Xorg and its GTK toolkits
* slim login manager

## Small warning

First thing to note is that you probably don't want to simply take the setup as
it is now.  Take a look what is started during X login. Comment everything out
that you don't want. This might be, for example, my "weird keyboard layout"
which makes a german keyboard to a better programmer's keyboard, but can be
annoying for start when you don't understand my intentions (and did not look up
what the layout actually does). I will talk about this below.

**Please be careful using this project. I cannot be hold responsible for any
damages that might occur. If you are not sure, STOP and DON'T USE anything
here.**

I tried my best to avoid dangerous operations during the setup and operation
phases (see below), but I cannot tell for sure if there are systems or hosts
that can be damaged by the actions executed there.

**TAKE A GOOD LOOK WHAT IS BEING DONE AND MAKE SURE THAT EVERYTHING IS
REASONABLE IN YOUR CASE! AND I MEAN E-V-E-R-Y-T-H-I-N-G!!**

## Installation instructions

If, at this point, you already don't understand something, give up please
before doing any damage.

For those who read the warnings above and have confidence that I am not
doing any harm, proceed with these steps:

0. **Make a backup of your dotfiles in $HOME**
1. Clone the project.
2. Move the project right to `~/.xmonad`.
3. When you have FreeBSD, you can directly execute the script
   `~/.xmonad/files_to_softlink/bootstrap.sh`. If not you will need
   to check the dependencies by yourself and comment out the
   `pkg info` check at the beginning of the script.
4. Optional: install the keyboard layout.

## Host-specific fine tuning

You can use the directory `~/.xmonad/conf` to make fine tuning for hosts.
Xmonad will look for a configuration there that is named
`~/.xmonad/conf/HOSTNAME.hs`.

### Workspaces and Layouts

The workspaces mentioned here have all 3 letters to keep the `dzen2` workspace
bar short. They are also merged with their number as prefix when shown in the
dzenbar.  You can use the names which are already defined to specify a certain
workspace layout.

The `com` layout is best used with `pidgin` and `claws-mail`. `gfx` is
adjusted for `gimp` other applications like `darktable` will be simply shown
full-screen there. Take a look at the layout definitions and don't forget
to modify them, especially when changing their names.

### Example

This configuration will make 9 desktops with the given names. It will take
`em0` as the interface to watch network load and on startup it will also
start firefox and claws-mail. It will also set the wallpaper with the
given arguments (just so you can see how to pass arguments).

```
HostConfiguration {
        workspaceNames = [ "web","com","dev","gfx","ofc","","irc","",""],
        netInterfaceName = "em0",
        autostartPrograms = [
		("hsetroot",["-fill","~/.wallpapers/liquid-1600x1200.jpg"]),
                ("firefox",[]),
                ("claws-mail",[])
                ]
        }
```

**Be careful here,** this has to be a valid Haskell record syntax (instance of
Read/Show) or it will be safely skipped and the defaults will be used.

#### Default host-specific configuration

It looks like this:
```
defaultHostConfiguration = HostConfiguration {
        workspaceNames = ["web","com","dev","gfx","ofc","","irc","",""],
        netInterfaceName = "re0",
        autostartPrograms = []
        }
```

You don't need to make a file for this, it is implied, if nothing has been
found or if an error reading your configuration occurs.

## Features and gotchas

The configuration is fully managed in [git](http://git-scm.com).

All the configurations included are softlinks. It means that you can update
them and you implicitely update the checked out project. This also implies
that it might be a good idea to make yourself a fork of this project. But
you can also merge in changes from my repository, but don't complain, if
something breaks for you. You should consider this project my private
playground.

## German hacker's keyboard layout

Because the layout file is very concise, I'll explain shortly what hides
inside. You can decide then, whether you like it or not.

**It is only for German keyboards!**

Here the features:

* US QWERTY setting (basically *it is* a US keyboard then)
* rich keyboard variant with many default key combos
	that exist in US (international) layouts
* Capslock and Escape swap (for vim)
* 'at' and 'euro' symbols back at their place
* Umlauts are accessible through *AltGr* plus the key where they are on the
	German keyboard. *Shift* will make them caps.
* Compose key on the "<>|" key next to left shift.
* `dead_greek` keyboard symbol on *AltGr*+g and *AltGr*+Shift+m
	to enable greek alphabet with default compositions
* some important dead keys have been swapped with their higher level siblings
	(e.g. `asciicircum`, `asciitilde`, `grave`)
* `dead_acute` on level 4 key `q` (think: "aQute")
* `dead_diaeresis` and `degree` on level 3 & 4 key `*`
* missing french quotation marks (because of umlauts) can be all composed
