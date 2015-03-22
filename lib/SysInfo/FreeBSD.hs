
module SysInfo.FreeBSD
        (startFreeBSD
        )
        where

import Dzen.Tools
import HostConfiguration
import System.IO
import XMonad

startFreeBSD :: String -> Int -> HostConfiguration -> X()
startFreeBSD homedir screenwidth conf = do
        io $ hPutStrLn stderr "Using FreeBSD status bar for this system."
        spawn $ freeBSDStatusBar homedir (netInterfaceName conf) screenwidth (longitude conf, latitude conf)
        return ()

freeBSDStatusBar :: String -> NetInterfaceName -> Int -> (Double, Double) -> String
freeBSDStatusBar homedir iface screenwidth (longitude, latitude) =
        "~/.xmonad/lib/Exec/FreeBSDStatusBar " ++ homedir ++
        " " ++ iface ++
        " " ++ show longitude ++
        " " ++ show latitude ++
        " | " ++ dzenStatusBar screenwidth

