= Contrib.Ssh

This module is basically [XMonad.Prompt.Ssh](https://github.com/xmonad/xmonad-contrib/blob/master/XMonad/Prompt/Ssh.hs)
with a slight modification of the function `sshPrompt` to allow passing a custom command with the ssh argument.

This is useful, if you need to start something inside the new window, like for example [tmux](https://github.com/tmux/tmux).

Usage example (taken from my Xmonad configuration):
```
((shiftMask .|. controlMask, xK_s),
        sshPrompt promptConfig (\p -> runInTerm "" $ "ssh -t " ++ p ++ " tmux -2 new-session"))
```
