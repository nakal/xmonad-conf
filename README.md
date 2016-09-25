# nakal's Xmonad (plus desktop) configuration

![Desktop screenshot](http://www.sugioarto.com/img/xmonad-screenshot.png)

[Xmonad](http://xmonad.org/) is a minimalistic tiled window manager for Xorg.

Please be aware that this repository does not just host an Xmonad
configuration, but also some additional goodies for a usable desktop.

Included fine tuning has been done for (e.g.):

* X settings:
	* Xorg Xdefaults and fonts
	* GTK+ 2 & 3
	* session management
	* dialogs for
		* SSH
		* rdesktop
		* VirtualBox
* login manager:
	* slim
* xmobar:
	* desktop bar
	* sysinfo bar

## Warning

The setup script used here will modify configurations and startup files
in your home directory. Please be aware of this fact!

I tried my best to avoid dangerous operations during the setup and operation
phases (see below), but I cannot tell for sure if there are systems or hosts
that can be damaged by the actions executed there.

When you start `setup.sh`, it will perform many safety checks not to destroy
important data.

## Prerequisites

`setup.sh` will check for required fonts and will ask you to install
[FontAwesome](http://fontawesome.io/). If your distribution does not offer the
font (FreeBSD does not), extract the `fonts` directory in
`font-awesome-a.b.c.zip` to a directory where your distribution usually
stores fonts. On FreeBSD it would be `/usr/local/share/fonts/FontAwesome`.
Then type:
```
cd /usr/local/share/fonts/FontAwesome
mkfontscale
mkfontdir
```

## Installation instructions

For those who read the warnings above, proceed with these steps:

0. **Make a backup of your $HOME** (optional, but you never know!)
1. Clone the project.
2. Move the project files to `~/.xmonad`.
3. When you have FreeBSD, you can directly execute the script
   `~/.xmonad/setup.sh`. If not you will need
   to check the dependencies by yourself and comment out the
   `pkg info` check at the beginning of the script.
4. Add this line (or similar) to your `sudo` configuration: `myuser ALL=(ALL) NOPASSWD: /sbin/shutdown`.
5. Optional: install the keyboard layout.

## Host-specific fine tuning

You can use the directory `~/.xmonad/conf` to make fine tuning for hosts.
Xmonad will look for a configuration there that is named
`~/.xmonad/conf/HOSTNAME.hs`.

### Workspaces and Layouts

The workspaces mentioned here have all 3 letters to keep the `xmobar` workspace
bar short. They are also merged with their number as prefix when shown in the
dock.  You can use the names which are already defined to specify a certain
workspace layout.

The `com` layout is best used with `pidgin` and `thunderbird`, `claws-mail` or
`mutt`. `gfx` is adjusted for `gimp` other applications like `darktable` will
be simply shown full-screen there. Take a look at the layout definitions and
don't forget to modify them, especially when changing their names.

### Example

This configuration will make 9 desktops with the given names. It will take
`em0` as the interface to watch network load and on startup it will also
start Firefox and Thunderbird. It will also set the wallpaper with the
given arguments (just so you can see how to pass arguments).

```
HostConfiguration {
	locale = "de",
        workspaceNames = [ "web","com","dev","gfx","ofc","","","",""],
        terminal = "urxvt",
        netInterfaceName = "em0",
        autostartPrograms = [
		("hsetroot",["-fill","~/.wallpapers/liquid-1600x1200.jpg"]),
                ("firefox",[]),
                ("thunderbird",[])
                ],
	ssh = [
		((64,115),"user@server1"),
		((65,115),"server2:222")
		]
        }
```

**Be careful here,** this has to be a valid Haskell record syntax (instance of
Read/Show) or it will be safely skipped and the defaults will be used.

Unfortunatelly, it is not possible to use `xK_s` and `mod`-Masks for
the keys in the configuration of `ssh` connections, so you will need to find the symbols
for yourself [here](http://xmonad.org/xmonad-docs/X11/src/Graphics-X11-Types.html).

#### Default host-specific configuration

It looks like this:
```
defaultHostConfiguration = HostConfiguration {
	locale = "en",
        workspaceNames = ["web","com","dev","gfx","ofc","","","",""],
        terminal = "urxvt",
        netInterfaceName = "re0",
        autostartPrograms = [],
	ssh = []
        }
```

You don't need to make a file for this, it is implied, if nothing has been
found or if an error reading your configuration occurs.

## Features and gotchas

The configuration is fully managed in [git](http://git-scm.com).

All the configurations included are softlinks. It means that you can update
them and you implicitly update the checked out project. This also implies
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
