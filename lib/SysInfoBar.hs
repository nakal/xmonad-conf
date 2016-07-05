
import Control.Concurrent
import Data.Char
import Data.Word
import System.BSD.Sysctl
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

type CPUUsed = Int
type CPUTotal = Int
data CPULoad = CPULoad CPUUsed CPUTotal

type MemTotal = Int
type MemFree = Int
data MemStat = MemStat MemTotal MemFree

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

-- getOpenBSDNetLoad :: String -> IO NetLoad
-- getOpenBSDNetLoad iface = do
--         str <- readProcess "/usr/bin/netstat" ["-i", "-I", iface, "-b"] []
--         let ls = tail $ lines str
--         if (null ls) then
--                 return $ NetLoad 0 0
--                 else
--                         let rr = words $ head ls
--                             rx = read $ rr !! 4
--                             tx = read $ rr !! 5
--                         in
--                                 return $ NetLoad rx tx

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

hotCPUColor :: Int -> String
hotCPUColor perc
        | perc < 33             = "lightblue"
        | perc < 66             = "orange"
        | otherwise             = "red"

hotMemColor :: MemStat -> String
hotMemColor memstat
        | perc < 60             = "lightblue"
        | perc < 80             = "orange"
        | otherwise             = "red"
        where perc = getMemPercent memstat

displayStats :: Handle -> Int -> MemStat -> (String,String) -> Int -> IO()
displayStats pipe cpuperc memstat (net_rx,net_tx) vol = do
        datestr <- getTimeAndDate
        hPutStrLn pipe $
                "<icon=cpu.xbm/><fc=" ++ hotCPUColor cpuperc ++ "> " ++ (show cpuperc) ++ "%</fc>   " ++
                "<icon=mem.xbm/><fc=" ++ hotMemColor memstat ++ "> " ++ (show $ getMemPercent memstat) ++ "%</fc>   " ++
                "<icon=net_wired.xbm/> " ++
                "<icon=net_down_03.xbm/> " ++ net_rx ++ "   " ++
                "<icon=net_up_03.xbm/> " ++ net_tx ++ "   " ++
                "<icon=volume.xbm/> " ++ (show vol) ++ "%   " ++
                "<fc=yellow>" ++ datestr ++ "</fc>"
        hFlush pipe

gatherLoop :: (OID, Int, OID) -> CPULoad -> (String -> IO NetLoad) -> IO Int -> Handle
        -> NetLoad -> String -> IO()
gatherLoop (oid_cpuload, memtotal, oid_memused) oldcpuload netloadfunc volfunc pipe lastnet iface = do
        cpuload <- getCPULoad oid_cpuload
        memstat <- getMemStat (memtotal, oid_memused)
        netload <- netloadfunc iface
        vol <- volfunc
        displayStats pipe (getCPUPercent (oldcpuload, cpuload)) memstat (getNetSpeeds (lastnet, netload)) vol
        threadDelay 1000000
        gatherLoop (oid_cpuload, memtotal, oid_memused) cpuload netloadfunc volfunc pipe netload iface

startBSD :: String -> Handle -> IO()
startBSD iface pipe = do
        oid_cpuload <- sysctlNameToOid "kern.cp_time"
        oid_memtotal <- sysctlNameToOid "vm.stats.vm.v_page_count"
        oid_memfree <- sysctlNameToOid "vm.stats.vm.v_free_count"
        cpuload <- getCPULoad oid_cpuload
        netinit <- getFreeBSDNetLoad iface
        memtotal <- sysctlReadUInt oid_memtotal
        gatherLoop (oid_cpuload, fromIntegral memtotal, oid_memfree) cpuload getFreeBSDNetLoad getVolume pipe netinit iface

getCPUPercent :: (CPULoad,CPULoad) -> Int
getCPUPercent (CPULoad oldused oldtotal, CPULoad curused curtotal) =
        let     deltatotal = curtotal - oldtotal
                deltaused = curused - oldused
                in if deltatotal > 0 then (100*deltaused) `div` deltatotal else 0

getMemPercent :: MemStat -> Int
getMemPercent (MemStat total free) = 100 - ((free * 100) `div` total)

getMemStat :: (Int, OID) -> IO MemStat
getMemStat (memtotal, oid_memfree) = do
        memfree <- sysctlReadUInt oid_memfree
        return $ MemStat memtotal (fromIntegral memfree)

getCPULoad :: OID -> IO CPULoad
getCPULoad oid_cpuload = do
        cpuloads2 <- sysctlPeekArray oid_cpuload :: IO [Word64]
        let     cpuloads = fmap fromIntegral cpuloads2
                total = sum cpuloads
                used = total - last cpuloads
                in return $ CPULoad used total

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
                                "freebsd" -> startBSD iface
                                -- "openbsd" -> startBSD iface
                                _         -> error $ "Unknown operating system " ++ os
                        )
                _              -> error "Error in parameters."
