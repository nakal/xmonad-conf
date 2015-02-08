
module SysInfo.StatusBar where

import Data.Char
import HostConfiguration
import System.Info
import SysInfo.Conky
import SysInfo.FreeBSD
import System.IO
import XMonad

startOSStatusBar :: String -> String -> Int -> HostConfiguration -> X()
startOSStatusBar os homedir barwidth conf
        -- | os == "freebsd" = startFreeBSD homedir barwidth conf
        | otherwise = startConky homedir barwidth conf

startStatusBar :: String -> Int -> HostConfiguration -> X()
startStatusBar homedir barwidth conf = do
        io $ hPutStrLn stderr $ "Starting status bar for " ++ os
        startOSStatusBar os homedir barwidth conf
