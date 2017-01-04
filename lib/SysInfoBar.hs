
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Either
import Data.Ratio
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
import qualified HostConfiguration as HC

type CPUUsed = Integer
type CPUTotal = Integer
data CPULoad = CPULoad CPUUsed CPUTotal

type NetRx = Integer
type NetTx = Integer
data NetLoad = NetLoad NetRx NetTx

type MemTotal = Integer
type MemFree = Integer
data MemStat = MemStat MemTotal MemFree

type SwapPercent = Integer

data Stats = Stats {
        locale :: String
        , slim :: Bool
        , pipe :: Handle
        , cpuPercent :: Integer
        , cpuCount :: Integer
        , memstat :: MemStat
        , swapperc :: SwapPercent
        , netload :: NetLoad
        , maybe_battime :: Maybe Integer
        }

myActiveColor = "#a8ff60"
myInactiveColor = "#606060"
myDefaultColor = "orange"
myMediumLoadColor = "yellow"
myHighLoadColor = "red"

getNetLoad :: Maybe Handle -> NetLoad -> IO NetLoad
getNetLoad (Just pipe) lastnetload = do
        ready <- catchIOError (hReady pipe) (\_ -> return False)
        if not ready then
                return lastnetload
        else do
                l <- fmap words $ hGetLine pipe
                getNetLoad (Just pipe) $ case readMaybe (head l) :: Maybe Integer of
                        Nothing -> lastnetload
                        _       -> NetLoad (read $ l !! 3) (read $ l !! 6)

getNetLoad _ lastnetload = return lastnetload

netspeed :: Integer -> String
netspeed x
        | x > 2 * 1024 ^ 3          =       printf "%.2fGB" (((fromIntegral x)/(1024^3)) :: Double)
        | x > 2 * 1024 ^ 2          =       printf "%.2fMB" (((fromIntegral x)/(1024^2)) :: Double)
        | x > 2 * 1024              =       printf "%.2fkB" (((fromIntegral x)/1024) :: Double)
        | otherwise                 =       printf "%d B " x

hotCPUColor :: Integer -> Integer -> String
hotCPUColor perc cpucount
        | perc < (100 `div` cpucount)           = myDefaultColor
        | perc < (100 `div` cpucount) * 2       = myMediumLoadColor
        | otherwise                             = myHighLoadColor

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

batteryColor :: Integer -> String
batteryColor minutes
        | minutes < 0              = myDefaultColor
        | minutes < 15             = myHighLoadColor
        | minutes < 30             = myMediumLoadColor
        | otherwise                = myDefaultColor

batteryIcon :: Integer -> String
batteryIcon minutes
        | minutes < 0              = "\xf1e6"
        | minutes < 15             = "\xf244"
        | minutes < 30             = "\xf243"
        | minutes < 90             = "\xf242"
        | minutes < 150            = "\xf241"
        | otherwise                = "\xf240"

batteryStats :: Maybe Integer -> String
batteryStats (Just battime) =
        printf " <fn=1>%v</fn><fc=%v>% 3vmin</fc>"
                (batteryIcon battime) (batteryColor battime)
                (if battime > 0 then show battime else "? ")
batteryStats _ = ""

displayStats :: Stats -> IO()
displayStats (Stats locale slim pipe cpuperc numcpu memstat swapperc (NetLoad net_rx net_tx) maybe_battime) = do
        datestr <- DF.getTimeAndDate locale slim
        hPutStrLn pipe $
                printf ("<fc=%v><fn=1>\xf142</fn></fc>  <fn=1>\xf0e4</fn>\
                        \<fc=%v>%3v%%</fc>   <fn=1>\xf00a\
                        \</fn><fc=%v>%3v%%</fc>   <fn=1>\xf1c0</fn><fc=%v>\
                        \%3v%%</fc>   <fn=1>\xf019</fn>\
                        \% 9v <fn=1>\xf093</fn>% 9v" ++ batteryStats maybe_battime ++ " <fc=%v><fn=1>\xf142\
                        \</fn></fc>  <fc=%v>%v</fc>")
                        myInactiveColor (hotCPUColor cpuperc numcpu) cpuperc
                        (hotMemColor memstat) (getMemPercent memstat)
                        (hotSwapColor swapperc) swapperc (netspeed net_rx)
                        (netspeed net_tx)
                        myInactiveColor myActiveColor datestr
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

gatherLoop :: String -> Bool -> (OID, Integer, Integer, OID, OID, Maybe OID) -> CPULoad -> Maybe Handle -> Handle
        -> NetLoad -> IO()
gatherLoop locale slim (oid_cpuload, numcpu, memtotal, oid_memfree, oid_meminact, oid_battime) oldcpuload netstatPipe pipe lastnet = do
        cpuload <- getCPULoad oid_cpuload
        memstat <- getMemStat (memtotal, oid_memfree, oid_meminact)
        netload <- getNetLoad netstatPipe lastnet
        swapload <- getSwapStats
        battime <- getBatteryTime oid_battime
        displayStats $ Stats locale slim pipe (getCPUPercent oldcpuload cpuload) numcpu memstat swapload netload battime
        threadDelay 1000000
        gatherLoop locale slim (oid_cpuload, numcpu, memtotal, oid_memfree, oid_meminact, oid_battime) cpuload netstatPipe pipe netload

startBSD :: String -> Bool -> Handle -> IO()
startBSD locale slim pipe = do
        [ oid_cpuload, oid_numcpu, oid_memtotal, oid_memfree, oid_meminact ]
                <- mapM sysctlNameToOid [ "kern.cp_time", "hw.ncpu", "vm.stats.vm.v_page_count", "vm.stats.vm.v_free_count", "vm.stats.vm.v_inactive_count" ]
        cpuload <- getCPULoad oid_cpuload
        oid_battime <- catchIOError (do
                oid <- sysctlNameToOid "hw.acpi.battery.time"
                return $ Just oid
                ) (\_ -> return Nothing)
        netstatPipe <- spawnNetStat
        netinit <- getNetLoad netstatPipe (NetLoad 0 0)
        [ numcpu, memtotal ] <- mapM sysctlReadUInt [ "hw.ncpu", "vm.stats.vm.v_page_count" ]
        gatherLoop locale slim (oid_cpuload, fromIntegral numcpu, fromIntegral memtotal, oid_memfree, oid_meminact, oid_battime)
                cpuload netstatPipe pipe netinit

getCPUPercent :: CPULoad -> CPULoad -> Integer
getCPUPercent (CPULoad oldused oldtotal) (CPULoad curused curtotal) =
        let     deltatotal = curtotal - oldtotal
                deltaused = curused - oldused
        in
                if deltatotal > 0 then (100*deltaused) `div` deltatotal else 0

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

getBatteryTime :: Maybe OID -> IO (Maybe Integer)
getBatteryTime (Just oid_battime) = do
        val <- sysctlReadInt oid_battime
        return $ Just (fromIntegral val)
getBatteryTime _ = return Nothing

spawnPipe :: [ String ] -> IO Handle
spawnPipe cmd = do
        (Just hin, _, _, _) <- createProcess (proc (head cmd) (tail cmd)){ std_in = CreatePipe }
        return hin

spawnNetStat :: IO (Maybe Handle)
spawnNetStat = do
        (_, Just hout, _, _) <- createProcess (proc "netstat"
                ["-ibW", "1"]){ std_out = CreatePipe }
        safeRead hout
        safeRead hout
        return $ Just hout
        where safeRead hout =
                catchIOError (do
                        line <- hGetLine hout
                        return $ Just line)
                        (\_ -> return Nothing)

xmobarSysInfo :: FilePath -> Bool -> [ String ]
xmobarSysInfo homedir slim =
        [ "xmobar", homedir ++ "/.xmonad/" ++
        (if slim then "slim_" else "")
        ++ "sysinfo_xmobar.rc" ]

main = do
        conf <- HC.readHostConfiguration
        homedir <- getHomeDirectory
        (spawnPipe $ xmobarSysInfo homedir (HC.slimView conf)) >>=
                (case os of
                        "freebsd" -> startBSD (HC.locale conf) (HC.slimView conf)
                        _         -> error $ "Unknown operating system " ++ os
                        )
