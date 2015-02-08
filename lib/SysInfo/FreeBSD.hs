
module SysInfo.FreeBSD where

import Control.Concurrent
import Data.Char
import System.Environment
import System.Process
import Text.Printf

type CPUUsed = Int
type CPUTotal = Int
data CPULoad = CPULoad CPUUsed CPUTotal
        deriving Show

type MemFree = Int
type MemTotal = Int
data MemLoad = MemLoad MemFree MemTotal

type NetRx = Int
type NetTx = Int
data NetLoad = NetLoad NetRx NetTx

getSysCtlCombinedValue :: String -> IO [ String ]
getSysCtlCombinedValue name =  fmap words $ readProcess "sysctl" [ "-n", name ] []

getSysCtlValues :: [ String ] -> IO [ String ]
getSysCtlValues names =  fmap lines $ readProcess "sysctl" ("-n":names) []

getCPULoad :: CPULoad -> IO CPULoad
getCPULoad oldload = do
        loadv <- getSysCtlCombinedValue "kern.cp_time"
        let ints = fmap (\x -> read x :: Int) loadv
            total = sum ints
            used = total - last ints
        return $ CPULoad used total

getMemLoad :: IO MemLoad
getMemLoad = do
        loadv <- getSysCtlValues [ "vm.stats.vm.v_page_count", "vm.stats.vm.v_free_count", "vm.stats.vm.v_inactive_count" ]
        let ints = fmap (\x -> read x :: Int) loadv
            total = head ints
            free = sum $ tail ints
        return $ MemLoad free total

getNetLoad :: String -> NetLoad -> IO NetLoad
getNetLoad iface (NetLoad oldrx oldtx) = do
        let (iface_name, iface_index) = splitNetInterfaceName iface
        let sysctlname_prefix = "dev." ++ iface_name ++ "." ++ iface_index ++ ".stats."
        loadv <- getSysCtlValues [ sysctlname_prefix ++ "rx.good_octets", sysctlname_prefix ++ "tx.good_octets" ]
        let ints = fmap (\x -> read x :: Int) loadv
        return $ NetLoad (head ints - oldrx) (last ints - oldtx)

getCPUPercent :: (CPULoad,CPULoad) -> Int
getCPUPercent (CPULoad oldused oldtotal, CPULoad curused curtotal) =
        let deltatotal = curtotal - oldtotal
            deltaused = curused - oldused
            in if deltatotal > 0 then (100*deltaused) `div` deltatotal else 0

getMemPercent :: MemLoad -> Int
getMemPercent (MemLoad free total) = (total - free) * 100 `div` total

getNetSpeeds :: NetLoad -> (String,String)
getNetSpeeds (NetLoad rx tx) = (netspeed rx, netspeed tx)

netspeed :: Int -> String
netspeed x
        | x > 2 * 1024 ^ 3          =       (printf "%.2f" (((fromIntegral x)/(1024^3)) :: Double)) ++ "GB"
        | x > 2 * 1024 ^ 2          =       (printf "%.2f" (((fromIntegral x)/(1024^2)) :: Double)) ++ "MB"
        | x > 2 * 1024              =       (printf "%.2f" (((fromIntegral x)/1024) :: Double)) ++ "kB"
        | otherwise                 =       (show x) ++ "B"

splitNetInterfaceName :: String -> (String, String)
splitNetInterfaceName iface = span isAlpha iface

-- main = do
--         setEnv "LC_NUMERIC" "C"
--         let iface = "alc0"
--         lastload <- getCPULoad $ CPULoad 0 0
--         lastnetload <- getNetLoad iface $ NetLoad 0 0
--         threadDelay 1000000
--         getCPULoad lastload >>= \load -> putStrLn $ show $ getCPUPercent (lastload,load)
--         getMemLoad >>= \memload -> putStrLn $ show $ getMemPercent memload
--         getNetLoad iface lastnetload >>= \netload -> putStrLn $ show $ getNetSpeeds netload
