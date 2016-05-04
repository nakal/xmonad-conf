
import Control.Concurrent
import Data.Char
import System.Directory
import System.Environment
import System.Exit
import System.Posix.Signals
import System.Process
import System.IO
import System.Info
import Text.Printf
import Text.Read

import qualified HostConfiguration as HC

type NetRx = Int
type NetTx = Int
data NetLoad = NetLoad NetRx NetTx

type CPUIdle = Int
type MemUsed = Int
type MemTotal = Int
type MemFree = Int
data VMStat = VMStatOpenBSD CPUIdle MemUsed MemFree | VMStatFreeBSD CPUIdle MemTotal MemFree

getFreeBSDNetLoad :: String -> IO NetLoad
getFreeBSDNetLoad iface = do
        str <- readProcess "/usr/bin/netstat" ["-i", "-I", iface, "-bW"] []
        let ls = tail $ lines str
        if (null ls) then
                return $ NetLoad 0 0
                else
                        let rr = words $ head ls
                            rx = read $ rr !! 7
                            tx = read $ rr !! 10
                        in
                                return $ NetLoad rx tx

getOpenBSDNetLoad :: String -> IO NetLoad
getOpenBSDNetLoad iface = do
        str <- readProcess "/usr/bin/netstat" ["-i", "-I", iface, "-b"] []
        let ls = tail $ lines str
        if (null ls) then
                return $ NetLoad 0 0
                else
                        let rr = words $ head ls
                            rx = read $ rr !! 4
                            tx = read $ rr !! 5
                        in
                                return $ NetLoad rx tx

getNetSpeeds :: (NetLoad,NetLoad) -> (String,String)
getNetSpeeds (NetLoad oldrx oldtx, NetLoad currx curtx) =
        (netspeed $ currx-oldrx, netspeed $ curtx - oldtx)

netspeed :: Int -> String
netspeed x
        | x > 2 * 1024 ^ 3          =       (printf "%.2f" (((fromIntegral x)/(1024^3)) :: Double)) ++ "GB"
        | x > 2 * 1024 ^ 2          =       (printf "%.2f" (((fromIntegral x)/(1024^2)) :: Double)) ++ "MB"
        | x > 2 * 1024              =       (printf "%.2f" (((fromIntegral x)/1024) :: Double)) ++ "kB"
        | otherwise                 =       (show x) ++ "B"

isNotTimezone :: String -> Bool
isNotTimezone str = not $ foldr (\x -> (&&) (isUpper x)) True str

endsWithDot :: String -> Bool
endsWithDot str = (length str > 0) && (last str) == '.'

filterSeconds :: String -> String
filterSeconds str =
        if fmap isDigit str == [True,True,False,True,True,False,True,True] &&
                fmap (== ':') str == [False,False,True,False,False,True,False,False] then
                        take 5 str else str

getTimeAndDate :: IO String
getTimeAndDate = do
        str <- fmap words $ readProcess "/bin/date" ["+%a %e %b %Y %H:%M"] []
        let f1 = fmap (\s -> if (endsWithDot s) then (init s) else s) $ fmap filterSeconds $ filter isNotTimezone str
        return $ unwords f1

getVolume :: IO Int
getVolume = do
        str <- readProcess "/usr/sbin/mixer" ["-S", "vol"] []
        let (left,d:right) = span (/= ':') $ drop 4 str
        return $ (read left + read right) `div` 2

hotCPUColor :: VMStat -> String
hotCPUColor vmstat
        | perc < 30             = "lightblue"
        | perc <= 80            = "orange"
        | otherwise             = "red"
        where perc = getCPUPercent vmstat

hotMemColor :: VMStat -> String
hotMemColor vmstat
        | perc < 60             = "lightblue"
        | perc < 80             = "orange"
        | otherwise             = "red"
        where perc = getMemPercent vmstat

displayStats :: Handle -> VMStat -> (String,String) -> Int -> IO()
displayStats pipe vmstat (net_rx,net_tx) vol = do
        datestr <- getTimeAndDate
        hPutStrLn pipe $
                "<icon=cpu.xbm/><fc=" ++ hotCPUColor vmstat ++ "> " ++ (show $ getCPUPercent vmstat) ++ "%</fc>   " ++
                "<icon=mem.xbm/><fc=" ++ hotMemColor vmstat ++ "> " ++ (show $ getMemPercent vmstat) ++ "%</fc>   " ++
                "<icon=net_wired.xbm/> " ++
                "<icon=net_down_03.xbm/> " ++ net_rx ++ "   " ++
                "<icon=net_up_03.xbm/> " ++ net_tx ++ "   " ++
                "<icon=volume.xbm/> " ++ (show vol) ++ "%   " ++
                "<fc=yellow>" ++ datestr ++ "</fc>"
        hFlush pipe

gatherLoop :: (String -> IO NetLoad) -> IO Int -> Handle -> Handle -> VMStat
        -> NetLoad -> String -> IO()
gatherLoop netloadfunc volfunc pipe vmstatpipe lastvmstat lastnet iface = do
        vmstat <- getVMStat vmstatpipe lastvmstat
        netload <- netloadfunc iface
        vol <- volfunc
        displayStats pipe vmstat (getNetSpeeds (lastnet, netload)) vol
        threadDelay 1000000
        gatherLoop netloadfunc volfunc pipe vmstatpipe vmstat netload iface

startFreeBSD :: String -> Handle -> IO()
startFreeBSD iface pipe = do
         vmstatpipe <- spawnVMStat [ "-H" ]
         vmstat <- getVMStat vmstatpipe (VMStatFreeBSD 0 0 0)
         netinit <- getFreeBSDNetLoad iface
         gatherLoop getFreeBSDNetLoad getVolume pipe vmstatpipe vmstat netinit iface

startOpenBSD :: String -> Handle -> IO()
startOpenBSD iface pipe = do
         vmstatpipe <- spawnVMStat []
         vmstat <- getVMStat vmstatpipe (VMStatOpenBSD 0 0 0)
         netload <- getOpenBSDNetLoad iface
         gatherLoop getOpenBSDNetLoad (return 0) pipe vmstatpipe vmstat netload iface

getCPUPercent :: VMStat -> Int
getCPUPercent (VMStatOpenBSD idle _ _) = 100 - idle
getCPUPercent (VMStatFreeBSD idle _ _) = 100 - idle

getMemPercent :: VMStat -> Int
getMemPercent (VMStatOpenBSD _ u f) = (u * 100) `div` f
getMemPercent (VMStatFreeBSD _ a f)
        | perc < 0      = 0
        | perc > 100    = 100
        | otherwise     =  perc
        where perc = ((a - f) * 100) `div` a

spawnVMStat :: [ String ] -> IO Handle
spawnVMStat args = do
        (_, Just hout, _, _) <- createProcess (proc "vmstat" $ args ++ ["1"]){ std_out = CreatePipe }
        hGetLine hout
        hGetLine hout
        return hout

getVMStat :: Handle -> VMStat -> IO VMStat
getVMStat vmstatpipe lastvmstat = do
        ready <- hReady vmstatpipe
        if not ready then
                return lastvmstat
                else do
                        l <- fmap words $ hGetLine vmstatpipe
                        getVMStat vmstatpipe $ case readMaybe (head l) :: Maybe Int of
                                Nothing -> lastvmstat
                                _       -> vmstattype (read $ last l) (read $ l !! 3) (read $ l !! 4)
        where vmstattype = case lastvmstat of
                VMStatOpenBSD _ _ _   ->      VMStatOpenBSD
                VMStatFreeBSD _ _ _   ->      VMStatFreeBSD

spawnPipe :: [ String ] -> IO Handle
spawnPipe cmd = do
        (Just hin, _, _, _) <- createProcess (proc (head cmd) (tail cmd)){ std_in = CreatePipe }
        return hin

xmobarSysInfo :: FilePath -> [ String ]
xmobarSysInfo homedir = [ "xmobar", homedir ++ "/.xmonad/sysinfo_xmobar.rc" ]

installSignals :: IO ()
installSignals = do
        ppid <- myThreadId
        mapM_ (\sig -> installHandler sig (Catch $ trap ppid) Nothing)
                [ lostConnection, keyboardSignal, softwareTermination, processStatusChanged ]

trap tid = do
        throwTo tid ExitSuccess

main = do
        homedir <- getHomeDirectory
        args <- getArgs
        case args of
                [ iface ]      -> spawnPipe (xmobarSysInfo homedir) >>= (
                        case os of
                                "freebsd" -> startFreeBSD iface
                                "openbsd" -> startOpenBSD iface
                                _         -> error $ "Unknown operating system " ++ os
                        )
                _              -> error "Error in parameters."
