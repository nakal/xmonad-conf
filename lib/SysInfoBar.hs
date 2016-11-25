
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Either
import Data.Word
import System.BSD.Sysctl
import System.Directory
import System.Environment
import System.Exit
import System.Posix.Signals
import System.Process
import System.IO
import System.IO.Error
import System.Info
import Text.Printf
import Text.Read

import qualified DateFormatter as DF

type NetRx = Integer
type NetTx = Integer
data NetLoad = NetLoad NetRx NetTx

type CPUUsed = Integer
type CPUTotal = Integer
data CPULoad = CPULoad CPUUsed CPUTotal

type MemTotal = Integer
type MemFree = Integer
data MemStat = MemStat MemTotal MemFree

type SwapPercent = Integer

myActiveColor = "#a8ff60"
myInactiveColor = "#606060"
myDefaultColor = "orange"
myMediumLoadColor = "yellow"
myHighLoadColor = "red"

getNetLoad :: Handle -> NetLoad -> IO NetLoad
getNetLoad pipe lastnetload = do
        ready <- hReady pipe
        if not ready then
                return lastnetload
        else do
                l <- fmap words $ hGetLine pipe
                getNetLoad pipe $ case readMaybe (head l) :: Maybe Integer of
                        Nothing -> lastnetload
                        _       -> NetLoad (read $ l !! 3) (read $ l !! 6)

netspeed :: Integer -> String
netspeed x
        | x > 2 * 1024 ^ 3          =       printf "%.2fGB" (((fromIntegral x)/(1024^3)) :: Double)
        | x > 2 * 1024 ^ 2          =       printf "%.2fMB" (((fromIntegral x)/(1024^2)) :: Double)
        | x > 2 * 1024              =       printf "%.2fkB" (((fromIntegral x)/1024) :: Double)
        | otherwise                 =       printf "%d B " x

isNotTimezone :: String -> Bool
isNotTimezone str = not $ foldr (\x -> (&&) (isUpper x)) True str

endsWithDot :: String -> Bool
endsWithDot str = (length str > 0) && (last str) == '.'

filterSeconds :: String -> String
filterSeconds str =
        if fmap isDigit str == [True,True,False,True,True,False,True,True] &&
                fmap (== ':') str == [False,False,True,False,False,True,False,False] then
                        take 5 str else str

hotCPUColor :: Integer -> String
hotCPUColor perc
        | perc < 33             = myDefaultColor
        | perc < 66             = myMediumLoadColor
        | otherwise             = myHighLoadColor

hotMemColor :: MemStat -> String
hotMemColor memstat
        | perc < 60             = myDefaultColor
        | perc < 80             = myMediumLoadColor
        | otherwise             = myHighLoadColor
        where perc = getMemPercent memstat

hotSwapColor :: Integer -> String
hotSwapColor perc
        | perc < 5              = myDefaultColor
        | perc < 20             = myMediumLoadColor
        | otherwise             = myHighLoadColor

displayStats :: String -> Handle -> Integer -> MemStat -> SwapPercent -> NetLoad -> IO()
displayStats locale pipe cpuperc memstat swapperc (NetLoad net_rx net_tx) = do
        datestr <- DF.getTimeAndDate locale
        hPutStrLn pipe $
                printf "<fc=%v><fn=1>\xf142</fn></fc>  <fn=1>\xf21e</fn>\
                        \<fc=%v>% 3v%%</fc>   <fn=1>\xf00a\
                        \</fn><fc=%v>% 3v%%</fc>   <fn=1>\xf1c0</fn><fc=%v>\
                        \% 3v%%</fc>   <fn=1>\xf019</fn>\
                        \% 11v <fn=1>\xf093</fn>% 11v <fc=%v><fn=1>\xf142\
                        \</fn></fc>  <fc=%v>%v</fc>"
                        myInactiveColor (hotCPUColor cpuperc) cpuperc
                        (hotMemColor memstat) (getMemPercent memstat)
                        (hotSwapColor swapperc) swapperc (netspeed net_rx)
                        (netspeed net_tx) myInactiveColor myActiveColor datestr
        hFlush pipe

getSwapStats :: IO SwapPercent
getSwapStats = do
        swap <- fmap rights $
                mapM (\nr -> tryIOError (
                        sysctlNameToOidArgs "vm.swap_info" [ nr ] >>=
                                sysctlPeekArray :: IO [Word32])) [0..15]
        let tot = sum $ fmap (!! 3) swap
            used = sum $ fmap (!! 4) swap
        return $ if tot > 0 then
                fromIntegral $ (used * 100) `div` tot
                else 0;

gatherLoop :: String -> (OID, Integer, OID, OID) -> CPULoad -> Handle -> Handle
        -> NetLoad -> IO()
gatherLoop locale (oid_cpuload, memtotal, oid_memfree, oid_meminact) oldcpuload netstatPipe pipe lastnet = do
        cpuload <- getCPULoad oid_cpuload
        memstat <- getMemStat (memtotal, oid_memfree, oid_meminact)
        netload <- getNetLoad netstatPipe lastnet
        swapload <- getSwapStats
        displayStats locale pipe (getCPUPercent (oldcpuload, cpuload)) memstat swapload netload
        threadDelay 1000000
        gatherLoop locale (oid_cpuload, memtotal, oid_memfree, oid_meminact) cpuload netstatPipe pipe netload

startBSD :: String -> String -> Handle -> IO()
startBSD locale iface pipe = do
        oid_cpuload <- sysctlNameToOid "kern.cp_time"
        oid_memtotal <- sysctlNameToOid "vm.stats.vm.v_page_count"
        oid_memfree <- sysctlNameToOid "vm.stats.vm.v_free_count"
        oid_meminact <- sysctlNameToOid "vm.stats.vm.v_inactive_count"
        netstatPipe <- spawnNetStat iface
        cpuload <- getCPULoad oid_cpuload
        netinit <- getNetLoad netstatPipe (NetLoad 0 0)
        memtotal <- sysctlReadUInt oid_memtotal
        gatherLoop locale (oid_cpuload, fromIntegral memtotal, oid_memfree, oid_meminact)
                cpuload netstatPipe pipe netinit

getCPUPercent :: (CPULoad,CPULoad) -> Integer
getCPUPercent (CPULoad oldused oldtotal, CPULoad curused curtotal) =
        let     deltatotal = curtotal - oldtotal
                deltaused = curused - oldused
                in if deltatotal > 0 then (100*deltaused) `div` deltatotal else 0

getMemPercent :: MemStat -> Integer
getMemPercent (MemStat total free) = 100 - ((free * 100) `div` total)

getMemStat :: (Integer, OID, OID) -> IO MemStat
getMemStat (memtotal, oid_memfree, oid_meminact) = do
        memfree <- sysctlReadUInt oid_memfree
        meminact <- sysctlReadUInt oid_meminact
        return $ MemStat memtotal (fromIntegral memfree + fromIntegral meminact)

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

spawnNetStat :: String -> IO Handle
spawnNetStat iface = do
        (_, Just hout, _, _) <- createProcess (proc "netstat"
                ["-i", "-I", iface, "-bW", "1"]){ std_out = CreatePipe }
        hGetLine hout
        hGetLine hout
        return hout

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
                [ locale, iface ]      -> spawnPipe (xmobarSysInfo homedir) >>= (
                        case os of
                                "freebsd" -> startBSD locale iface
                                _         -> error $ "Unknown operating system " ++ os
                        )
                _              -> error "Error in parameters. Need locale and network interface."
