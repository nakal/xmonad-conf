
import Control.Concurrent
import Data.Char
import Data.Time
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

type CPUUsed = Int
type CPUTotal = Int
data CPULoad = CPULoad CPUUsed CPUTotal

type MemFree = Int
type MemTotal = Int
data MemLoad = MemLoad MemFree MemTotal

type NetRx = Int
type NetTx = Int
data NetLoad = NetLoad NetRx NetTx

type CPUIdle = Int
type MemUsed = Int
data VMStat = VMStat CPUIdle MemUsed MemFree

getSysCtlCombinedValue :: String -> IO [ String ]
getSysCtlCombinedValue name =  fmap words $ readProcess "/sbin/sysctl" [ "-n", name ] []

getSysCtlValues :: [ String ] -> IO [ String ]
getSysCtlValues names =  fmap lines $ readProcess "/sbin/sysctl" ("-n":names) []

getCPULoad :: IO CPULoad
getCPULoad = do
        loadv <- getSysCtlCombinedValue "kern.cp_time"
        return $ getSingleCPULoad loadv

getSingleCPULoad :: [ String ] -> CPULoad
getSingleCPULoad xs =
        let ints = fmap (\x -> read x :: Int) xs
            total = sum ints
            used = total - last ints
                in CPULoad used total

allCoreLoads :: IO [ CPULoad ]
allCoreLoads = do
        loadv <- getSysCtlCombinedValue "kern.cp_times"
        return $ splitCPULoads loadv

getBusyCPUs :: ([ CPULoad ], [ CPULoad ] ) -> (Int,Int)
getBusyCPUs (old,cur) =
        (foldr (\x -> (+) (if x then 1 else 0)) 0 $ fmap isbusy $ fmap getCPUPercent (zip old cur), length cur)
        where isbusy perc = perc >= 90

splitCPULoads :: [ String ] -> [ CPULoad ]
splitCPULoads [] = []
splitCPULoads xs =
        (getSingleCPULoad $ take 5 xs) : (splitCPULoads $ drop 5 xs)

getMemLoad :: IO MemLoad
getMemLoad = do
        loadv <- getSysCtlValues [ "vm.stats.vm.v_page_count", "vm.stats.vm.v_free_count", "vm.stats.vm.v_inactive_count" ]
        let ints = fmap (\x -> read x :: Int) loadv
            total = head ints
            free = sum $ tail ints
        return $ MemLoad free total

getNetLoad :: String -> IO NetLoad
getNetLoad iface = do
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

getCPUPercent :: (CPULoad,CPULoad) -> Int
getCPUPercent (CPULoad oldused oldtotal, CPULoad curused curtotal) =
        let deltatotal = curtotal - oldtotal
            deltaused = curused - oldused
            in if deltatotal > 0 then (100*deltaused) `div` deltatotal else 0

getMemPercent :: MemLoad -> Int
getMemPercent (MemLoad free total) = (total - free) * 100 `div` total

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

hotCPUColor :: (Int,Int) -> String
hotCPUColor (hot,total)
        | hot == 0              = "lightblue"
        | hot <= total `div` 2  = "orange"
        | otherwise             = "red"

hotMemColor :: Int -> String
hotMemColor perc
        | perc < 60             = "lightblue"
        | perc < 80             = "orange"
        | otherwise             = "red"

displayStats :: Handle -> Int -> (Int,Int) -> Int -> (String,String) -> Int -> IO()
displayStats pipe cpu coreloads mem (net_rx,net_tx) vol = do
        datestr <- getTimeAndDate
        hPutStrLn pipe $
                "<icon=cpu.xbm/><fc=" ++ hotCPUColor coreloads ++ "> " ++ (show cpu) ++ "%</fc>   " ++
                "<icon=mem.xbm/><fc=" ++ hotMemColor mem ++ "> " ++ (show mem) ++ "%</fc>   " ++
                "<icon=net_wired.xbm/> " ++
                "<icon=net_down_03.xbm/> " ++ net_rx ++ "   " ++
                "<icon=net_up_03.xbm/> " ++ net_tx ++ "   " ++
                "<icon=volume.xbm/> " ++ (show vol) ++ "%   " ++
                "<fc=yellow>" ++ datestr ++ "</fc>"
        hFlush pipe

gatherLoopFreeBSD :: Handle -> TimeZone -> CPULoad -> [ CPULoad ]
        -> NetLoad -> String -> IO()
gatherLoopFreeBSD pipe tz lastcpu lastcoreloads lastnet iface = do
        cpuload <- getCPULoad
        coreloads <- allCoreLoads
        mem <- fmap getMemPercent getMemLoad
        netload <- getNetLoad iface
        vol <- getVolume
        displayStats pipe (getCPUPercent (lastcpu,cpuload))
                (getBusyCPUs (lastcoreloads,coreloads)) mem
                (getNetSpeeds (lastnet, netload)) vol
        threadDelay 1000000
        gatherLoopFreeBSD pipe tz cpuload coreloads netload iface

startFreeBSD :: String -> Handle -> IO()
startFreeBSD iface pipe = do
         -- setEnv "LC_NUMERIC" "C"
         tz <- getCurrentTimeZone
         cpuinit <- getCPULoad
         coreloadsinit <- allCoreLoads
         netinit <- getNetLoad iface
         gatherLoopFreeBSD pipe tz cpuinit coreloadsinit netinit iface

getOpenBSDCPUPercent :: VMStat -> Int
getOpenBSDCPUPercent (VMStat id _ _) = 100 - id

getHotness :: VMStat -> (Int, Int)
getHotness (VMStat id _ _)
        | id > 66              = (0, 2)
        | id > 33              = (1, 2)
        | otherwise            = (2, 2)

getOpenBSDMemPercent :: VMStat -> Int
getOpenBSDMemPercent (VMStat _ u f) = (u * 100) `div` f

gatherLoopOpenBSD :: Handle -> TimeZone -> Handle -> VMStat -> NetLoad -> String -> IO()
gatherLoopOpenBSD pipe tz vmstatpipe lastvmstat lastnet iface = do
        vmstat <- getVMStat vmstatpipe lastvmstat
        netload <- getOpenBSDNetLoad iface
        displayStats pipe (getOpenBSDCPUPercent vmstat) (getHotness vmstat)
                (getOpenBSDMemPercent vmstat) (getNetSpeeds (lastnet, netload)) 0
        threadDelay 1000000
        gatherLoopOpenBSD pipe tz vmstatpipe vmstat netload iface

spawnVMStat :: IO Handle
spawnVMStat = do
        (_, Just hout, _, _) <- createProcess (proc "vmstat" ["1"]){ std_out = CreatePipe }
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
                                _       -> VMStat (read $ last l) (read $ l !! 3) (read $ l !! 4)

startOpenBSD :: String -> Handle -> IO()
startOpenBSD iface pipe = do
         tz <- getCurrentTimeZone
         vmstatpipe <- spawnVMStat
         vmstat <- getVMStat vmstatpipe (VMStat 0 0 0)
         netload <- getOpenBSDNetLoad iface
         gatherLoopOpenBSD pipe tz vmstatpipe vmstat netload iface

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
