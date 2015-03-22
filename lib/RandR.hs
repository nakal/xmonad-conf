
module RandR
        (updateDisplayLevel
        ,xRandrOutputs
        ,XRandrOutput
        )
        where

import Data.Time.Horizon
import Data.Time
import Data.Time.LocalTime
import System.Process

{-
 - night:       0.4 (>1h before sunrise)
 - morning:     0.4-1.0 (0..60min before sunrise)
 - day:         1.0 (<1h after sunset)
 - evening:     1.0-0.4 (1h..2h after sunset)
 - night:       0.4 (>2h after sunset)
 -}

morningTime :: Int
morningTime = -20

morningDuration :: Int
morningDuration = 30

eveningTime :: Int
eveningTime = 20

eveningDuration :: Int
eveningDuration = 30

type XRandrOutput = String

dayMinutes :: LocalTime -> Int
dayMinutes time =
        let tod = localTimeOfDay time
            (hours, minutes) = (todHour tod, todMin tod)
        in
                hours * 60 + minutes

getSunTime :: TimeZone -> LongitudeWest -> LatitudeNorth -> UTCTime ->
        (Day -> LongitudeWest -> LatitudeNorth -> UTCTime) -> LocalTime
getSunTime tz long lat time f =
        locTim $ f (localDay $ locTim time) long lat
        where locTim = utcToLocalTime tz

getLightLevel :: (Int, Int, Int) -> Int
getLightLevel (now, daystart, dayend)
        | now < daystart + morningTime  = 4
        | now < daystart                = 10 - div ((daystart - now) * 6) morningDuration
        | now > dayend + eveningTime + eveningDuration = 4
        | now > dayend + eveningTime    = 4 + div ((dayend + eveningTime + eveningDuration - now) * 6) eveningDuration
        | otherwise                     = 10

xRandrSetLevel :: Int -> String -> IO ()
xRandrSetLevel lev x_out = do
        rawSystem "xrandr" [ "--output", x_out, "--gamma", "1:1:" ++
                (level_str lev), "--brightness", level_str lev ] >>= \_ -> return ()
        where level_str lev
                | lev >= 10     = "1.0"
                | otherwise     = "0." ++ show lev

xRandrOutputs :: IO [ XRandrOutput ]
xRandrOutputs = do
        ls <- fmap lines $ readProcess "/usr/local/bin/xrandr" [] []
        let     wdls = fmap words ls
                fl = filter (\x -> 2 <= (length  x)) wdls
                co = fmap (\ws -> (head ws, ws !! 1)) fl
        return $ fmap fst $ filter (\p -> (==) "connected" (snd p)) co

updateDisplayLevel :: TimeZone -> LongitudeWest -> LatitudeNorth -> [ XRandrOutput ] -> IO ()
updateDisplayLevel tz long lat x_Outputs = do
        now <- getCurrentTime
        let localtime = utcToLocalTime tz now
        let [ sunrise_time, sunset_time ] =
                fmap (dayMinutes . getSunTime tz long lat now) [ sunrise, sunset ]
        mapM_ (xRandrSetLevel $ getLightLevel (dayMinutes localtime, sunrise_time, sunset_time)) x_Outputs

-- main = do
--         tz <- getCurrentTimeZone
--         x_Outputs <- xRandrOutputs
--         putStrLn $ "Time now: " ++ show localtime
--         putStrLn $ "Day minutes: " ++ show (dayMinutes localtime)
--         putStrLn $ "Sunrise today: " ++ show sunrise_time
--         putStrLn $ "Sunset today: " ++ show sunset_time
--         putStrLn $ "Light level: " ++ show (getLightLevel (dayMinutes localtime, sunrise_time, sunset_time))
