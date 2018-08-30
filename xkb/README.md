# German hacker's keyboard layout

## Why you would want to use this layout?

1. You use German keyboards.
2. You prefer the US layout.
3. You use are writing code most of your time.
4. You use Vim and hate to reach for the *Esc* key.
5. You would like to maximize the amount of available key mappings.

## How to install and use it?

Easy!

1. Locate your `symbols` directory of the Xorg installation. It's typically
   `/usr/local/share/X11/xkb/symbols` or `/usr/share/X11/xkb/symbols`.
2. Copy the [`us_alt`](us_alt) file to this directory.
3. As user, load it with `setxkbmap us_alt`.

## What is inside?

Because the layout file is very concise, I'll explain shortly what hides
inside. You can decide then, whether you like it or not.

These are the features:

* based on the US QWERTY layout
* extended with international layout option (uses many levels!)
  * level 1 = the key itself
  * level 2 = *Shift* + key
  * level 3 = *AltGr* + key
  * level 4 = *AltGr* + *Shift* + key
* Capslock and Escape swap (for Vim)
* 'at' and 'euro' symbols back at their place
* Umlauts are accessible through *AltGr* plus the key where they are on the
	German keyboard. *Shift* will make them caps.
* *Compose* key (aka `Multi_key`) on the "<>|" key next to left shift
  * in case you don't know it *Compose* is pressed in sequence with two
    other keys which will "compose" a single char
  * see documentation in file `/usr/local/lib/X11/locale/en_US.UTF-8/Compose` or
    `/usr/lib/X11/locale/en_US.UTF-8/Compose`
* `dead_greek` keyboard symbol on *AltGr*+g and *AltGr*+Shift+m
	to enable greek alphabet with default compositions
* some important dead keys have been swapped with their higher level siblings
	(e.g. `asciicircum`, `asciitilde`, `grave`)
* `dead_acute` on level 4 key `q` (think: "aQute")
* `dead_diaeresis` and `degree` on level 3 & 4 key `+`
* missing french quotation marks (because of umlauts) can be simply composed
  * see *Compose* `<` `<` and *Compose* `>` `>`
