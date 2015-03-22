
module SysInfo.StatusBar
        (startStatusBar
        )
        where

import Data.Char
import HostConfiguration
import System.Info
import SysInfo.Conky
import SysInfo.FreeBSD
import System.IO
import XMonad
import XMonad.Core

startOSStatusBar :: String -> String -> Int -> HostConfiguration -> X()
startOSStatusBar os homedir screenwidth conf
        | os == "freebsd" = startFreeBSD homedir screenwidth conf
        | otherwise = startConky homedir screenwidth conf

startStatusBar :: String -> Int -> HostConfiguration -> X()
startStatusBar homedir barwidth conf = do
        io $ hPutStrLn stderr $ "Starting status bar for " ++ os
        startOSStatusBar os homedir barwidth conf
