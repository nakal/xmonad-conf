
module Dzen.Tools where

import HostConfiguration

dzenBitmap :: FilePath -> String -> String
dzenBitmap homedir pic = homedir ++ "/.xmonad/dzen2/" ++ pic ++ ".xbm"

dzenFont :: String
dzenFont = "Bitstream Vera Sans Mono:size=11:bold"

dzenExec :: String
dzenExec = "dzen2 -e '' -y 0 -h 24 -fg '#FFFFFF' -bg '#202020' -fn '" ++ dzenFont ++ "'"

myXmonadBarWidth :: Int -> Int
myXmonadBarWidth screenwidth = screenwidth - myStatusBarWidth screenwidth

myStatusBarWidth :: Int -> Int
myStatusBarWidth screenwidth = screenwidth `div` 3
myStatusBarXOfs :: Int -> Int
myStatusBarXOfs = myXmonadBarWidth
