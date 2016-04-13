
module Dzen.Tools where

import HostConfiguration

dzenBitmap :: FilePath -> String -> String
dzenBitmap homedir pic = homedir ++ "/.xmonad/dzen2/" ++ pic ++ ".xbm"

dzenFont :: String
dzenFont = "Inconsolata:size=12:bold"

dzenExec :: String
dzenExec = "dzen2 -e '' -y 0 -h 20 -fg '#FFFFFF' -bg '#202020' -fn '" ++ dzenFont ++ "'"

myXmonadBarWidth :: Int -> Int
myXmonadBarWidth screenwidth = screenwidth - myStatusBarWidth screenwidth

myStatusBarWidthMinimal :: Int
myStatusBarWidthMinimal = 580

myStatusBarWidth :: Int -> Int
myStatusBarWidth screenwidth =
        if onethird < myStatusBarWidthMinimal
                then myStatusBarWidthMinimal else onethird
        where onethird = screenwidth `div` 3
myStatusBarXOfs :: Int -> Int
myStatusBarXOfs = myXmonadBarWidth

dzenStatusBar :: Int -> String
dzenStatusBar screenwidth =
        dzenExec ++ " -x " ++ (show $ myStatusBarXOfs screenwidth) ++
        " -w " ++ (show $ myStatusBarWidth screenwidth)
