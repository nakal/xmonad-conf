
module SysInfo.Conky where

import Dzen.Tools
import HostConfiguration
import System.IO
import XMonad

panelLine :: Int -> FilePath -> String -> String
panelLine width homedir netif =
        "^fg(white) " ++
        "^pa(" ++ panelofs 560 ++ ") |  ^fg(lightblue)" ++ ( bitmap "cpu" ) ++ " ${cpu}% " ++
	"^pa(" ++ panelofs 470 ++ ") " ++ ( bitmap "mem" ) ++ " ${memperc}% " ++
	"^pa(" ++ panelofs 405 ++ ") " ++ ( bitmap "net_wired" ) ++ " " ++
	"^pa(" ++ panelofs 390 ++ ") " ++ ( bitmap "net_down_03" ) ++ "${downspeed " ++ netif ++ "} " ++
	"^pa(" ++ panelofs 315 ++ ") " ++ ( bitmap "net_up_03" ) ++ "${upspeed " ++ netif ++ "} " ++
	"^pa(" ++ panelofs 240 ++ ") " ++ ( bitmap "volume" ) ++ " ${exec mixer vol | egrep -o \"[0-9]+$\" | head -1 | egrep -o \"[0-9]*\"}% " ++
	"^fg(yellow) ^pa(" ++ panelofs 180 ++ ") ${time %a %d.%m.%Y} ${time %R}"
	where bitmap name = "^i(" ++ (dzenBitmap homedir name) ++ ")"
              panelofs x = show $ width - x

writeConkyConf :: Int -> FilePath -> NetInterfaceName -> IO ()
writeConkyConf width homedir netif =
	writeFile (homedir ++ "/.xmonad/conkyrc") (unlines $ conf homedir netif)
		where conf homedir netif = [
				"background yes",
				"out_to_console yes",
				"out_to_x no",
				"update_interval 1",
				"",
				"TEXT",
				panelLine width homedir netif
			]

startConky :: String -> Int -> HostConfiguration -> X()
startConky homedir screenwidth conf = do
        io $ hPutStrLn stderr "Using conky for this system."
        io $ writeConkyConf (myStatusBarWidth screenwidth) homedir (netInterfaceName conf)
        spawn (myStatusBar homedir screenwidth)
        return ()

-- right hand side, resources, date, time
myStatusBar :: String -> Int -> String
myStatusBar myHomeDirectory screenwidth = "conky -c " ++ myHomeDirectory
        ++ "/.xmonad/conkyrc | " ++
        dzenExec ++ " -x " ++ (show $ myStatusBarXOfs screenwidth) ++
        " -w " ++ (show $ myStatusBarWidth screenwidth)

