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
		* [SSH](lib/Contrib/README.md)
		* rdesktop
		* VirtualBox
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

## Installation instructions

For those who read the warnings above, proceed with these steps:

0. **Make a backup of your $HOME** (optional, but you never know!)
1. Clone the project.
2. Move the project files to `~/.xmonad`.
3. If you have FreeBSD, you can directly execute the script
   `~/.xmonad/setup.sh`. If not you will need
   to check the dependencies by yourself and comment out the
   `pkg info` check at the beginning of the script.
4. Add this line (or similar) to your `sudo` configuration: `myuser ALL=(ALL) NOPASSWD: /sbin/shutdown`.
5. Optional: install the keyboard layout.

## Host-specific fine tuning

You can use the directory `~/.xmonad/conf` to make fine tuning for hosts.
Xmonad will look for a configuration there that is named
`~/.xmonad/conf/HOSTNAME.json`. It is stored in
[JSON](https://www.json.org/json-en.html) format.

### Workspaces and Layouts

The workspaces mentioned here have all 3 letters to keep the `xmobar` workspace
bar short. They are also merged with their number as prefix when shown in the
dock.  You can use the names which are already defined to specify a certain
workspace layout.

The `com` layout is best used with `pidgin` and `thunderbird`, `claws-mail` or
`mutt`. `gfx` is adjusted for `gimp` other applications like `darktable` will
be simply shown full-screen there. Take a look at the layout definitions and
don't forget to modify them, especially when changing their names.

Use F-keys to navigate to the workspaces or click on the workspace bar.

### Netbooks and devices with small screens

If you have a small screen, you can set `slimscreen = true` in your
host configuration file's `[general]` section.

### Example

This configuration will make 9 workspaces with the given names.  It will also
start Firefox and Thunderbird. It will also set the wallpaper with the given
arguments (just so you can see how to pass arguments).

```
{
        "general":{
                "locale":"de",
                "terminal":"xterm",
                "slimscreen":false
        },

        "workspaces":[
                "web", "com", "dev", "gfx", "ofc"
        ],

        "autostart":[
                ["hsetroot", "-fill", "~/.wallpapers/my-wallpaper.jpg"]
                , ["firefox"]
                , ["thunderbird"]
        ],

        "mapping":[
                {
                        "key":"M-s",
                        "name":"ssh user@server1",
                        "exec": [ "ssh", "-Y", "-t", "user@server1", "tmux -2 new-session" ],
                        "in_terminal":true
                },
                {
                        "key":"M-S-s",
                        "name":"ssh server2",
                        "exec": [ "ssh", "-p", "222", "-Y", "-t", "server2", "tmux -2 new-session" ],
                        "in_terminal":true
                }
        ]
}
```

Problem reading the configuration file are logged in `~/.xsession-errors`.
For most cases of problems the appropriate defaults will be used.

#### Default host-specific configuration

It looks like this:
```
{
        "general":{
                "locale":"en",
                "terminal":"xterm",
                "slimscreen":false
        },

        "workspaces":[
                "web", "com", "dev", "gfx", "ofc"
        ],

        "autostart":[],

        "mapping":[]
}
```

You don't need to make a file for this, it is implied, if no configuration has been
found for the current host or if an error reading your configuration occurs.

## Features and gotchas

The configuration is fully managed in [git](http://git-scm.com).

All the configurations included are softlinks. It means that you can update
them and you implicitly update the checked out project. This also implies
that it might be a good idea to have a fork of this project for yourself. But
you can also merge in changes from my repository, but don't complain, if
something breaks for you. You should consider this project my private
playground.

## German hacker's keyboard layout

See [description here](xkb/README.md).
