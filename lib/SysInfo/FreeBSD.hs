
module SysInfo.FreeBSD
        (startFreeBSD
        )
        where

import Dzen.Tools
import HostConfiguration
import SysInfo.StatusBarType
import System.IO
import XMonad

startFreeBSD :: String -> Int -> HostConfiguration -> X StatusBarType
startFreeBSD homedir screenwidth conf = do
        io $ hPutStrLn stderr "Using FreeBSD status bar for this system."
        spawn $ freeBSDStatusBar homedir (netInterfaceName conf) screenwidth
        return FreeBSDBar

freeBSDStatusBar :: String -> NetInterfaceName -> Int -> String
freeBSDStatusBar homedir iface screenwidth =
        "~/.xmonad/lib/Exec/FreeBSDStatusBar " ++ homedir ++
        " " ++ iface ++
        " | " ++ dzenStatusBar screenwidth

