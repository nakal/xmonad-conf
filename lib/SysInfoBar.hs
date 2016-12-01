
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
        , cpuload :: Rational
        , cpucount :: Integer
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

hotCPUColor :: Rational -> Integer -> String
hotCPUColor cpuload numcpu
        | cpuload >= numcpu % 1 = myHighLoadColor
        | cpuload >= numcpu % 2 = myMediumLoadColor
        | otherwise             = myDefaultColor

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
displayStats (Stats locale slim pipe cpuload numcpu memstat swapperc (NetLoad net_rx net_tx) maybe_battime) = do
        datestr <- DF.getTimeAndDate locale slim
        hPutStrLn pipe $
                printf ("<fc=%v><fn=1>\xf142</fn></fc>  <fn=1>\xf0e4</fn>\
                        \<fc=%v>% .2f</fc>   <fn=1>\xf00a\
                        \</fn><fc=%v>%3v%%</fc>   <fn=1>\xf1c0</fn><fc=%v>\
                        \%3v%%</fc>   <fn=1>\xf019</fn>\
                        \% 9v <fn=1>\xf093</fn>% 9v" ++ batteryStats maybe_battime ++ " <fc=%v><fn=1>\xf142\
                        \</fn></fc>  <fc=%v>%v</fc>")
                        myInactiveColor (hotCPUColor cpuload numcpu) (fromRational cpuload :: Float)
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

gatherLoop :: String -> Bool -> (OID, Integer, Integer, OID, OID, Maybe OID) -> Maybe Handle -> Handle
        -> NetLoad -> IO()
gatherLoop locale slim (oid_vmload, numcpu, memtotal, oid_memfree, oid_meminact, oid_battime) netstatPipe pipe lastnet = do
        cpuload <- getCPULoad oid_vmload
        memstat <- getMemStat (memtotal, oid_memfree, oid_meminact)
        netload <- getNetLoad netstatPipe lastnet
        swapload <- getSwapStats
        battime <- getBatteryTime oid_battime
        displayStats $ Stats locale slim pipe cpuload numcpu memstat swapload netload battime
        threadDelay 1000000
        gatherLoop locale slim (oid_vmload, numcpu, memtotal, oid_memfree, oid_meminact, oid_battime) netstatPipe pipe netload

startBSD :: String -> Bool -> String -> Handle -> IO()
startBSD locale slim iface pipe = do
        [ oid_vmload, oid_numcpu, oid_memtotal, oid_memfree, oid_meminact ]
                <- mapM sysctlNameToOid [ "vm.loadavg", "hw.ncpu", "vm.stats.vm.v_page_count", "vm.stats.vm.v_free_count", "vm.stats.vm.v_inactive_count" ]
        oid_battime <- catchIOError (do
                oid <- sysctlNameToOid "hw.acpi.battery.time"
                return $ Just oid
                ) (\_ -> return Nothing)
        netstatPipe <- spawnNetStat iface
        netinit <- getNetLoad netstatPipe (NetLoad 0 0)
        [ numcpu, memtotal ] <- mapM sysctlReadUInt [ "hw.ncpu", "vm.stats.vm.v_page_count" ]
        gatherLoop locale slim (oid_vmload, fromIntegral numcpu, fromIntegral memtotal, oid_memfree, oid_meminact, oid_battime)
                netstatPipe pipe netinit

getMemPercent :: MemStat -> Integer
getMemPercent (MemStat total free) = 100 - ((free * 100) `div` total)

getMemStat :: (Integer, OID, OID) -> IO MemStat
getMemStat (memtotal, oid_memfree, oid_meminact) = do
        memfree <- sysctlReadUInt oid_memfree
        meminact <- sysctlReadUInt oid_meminact
        return $ MemStat memtotal (fromIntegral memfree + fromIntegral meminact)

getCPULoad :: OID -> IO Rational
getCPULoad oid_cpuload = do
        cpuloads_raw <- sysctlPeekArray oid_cpuload :: IO [Word32]
        let cpuloads = dropZeroSuffix cpuloads_raw in
                return $ (fromIntegral $ head cpuloads) % (fromIntegral $ last cpuloads)
        where dropZeroSuffix elems
                | last elems == 0       =       dropZeroSuffix $ init elems
                | otherwise             =       elems

getBatteryTime :: Maybe OID -> IO (Maybe Integer)
getBatteryTime (Just oid_battime) = do
        val <- sysctlReadInt oid_battime
        return $ Just (fromIntegral val)
getBatteryTime _ = return Nothing

spawnPipe :: [ String ] -> IO Handle
spawnPipe cmd = do
        (Just hin, _, _, _) <- createProcess (proc (head cmd) (tail cmd)){ std_in = CreatePipe }
        return hin

spawnNetStat :: String -> IO (Maybe Handle)
spawnNetStat iface = do
        (_, Just hout, _, _) <- createProcess (proc "netstat"
                ["-i", "-I", iface, "-bW", "1"]){ std_out = CreatePipe }
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
        homedir <- getHomeDirectory
        args <- getArgs
        case args of
                [ locale, iface, slimstr ]      ->
                        let slim = read slimstr :: Bool in
                        spawnPipe (xmobarSysInfo homedir slim) >>= (
                        case os of
                                "freebsd" -> startBSD locale slim iface
                                _         -> error $ "Unknown operating system " ++ os
                        )
                _              -> error "Error in parameters. Need locale and network interface."
