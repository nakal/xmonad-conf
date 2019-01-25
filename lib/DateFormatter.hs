module DateFormatter
        ( getTimeAndDate
        ) where

import Data.Time.Format
import Data.Time.LocalTime

import qualified HostConfiguration as HC

wantSeconds :: HC.HostConfiguration -> String
wantSeconds hc = if HC.isSlim hc then "" else ":%S"

getTimeLocale :: String -> HC.HostConfiguration -> TimeLocale
getTimeLocale "de" conf = TimeLocale {
        wDays = fmap (\x -> (x, take 2 x)) [ "Sonntag", "Montag", "Dienstag",
                "Mittwoch", "Donnerstag", "Freitag", "Samstag" ],
        months = fmap (\x -> (x, take 3 x)) [ "Januar", "Februar", "März",
                "April", "Mai", "Juni", "Juli", "August", "September",
                "Oktober", "November", "Dezember" ],
        amPm = ("früh", "nachm."),
        dateTimeFmt = "%a %e %b %Y %H:%M" ++ wantSeconds conf,
        dateFmt = "%a, den %e. %B %Y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%l Uhr %-zM %P",
        knownTimeZones = [
                TimeZone { timeZoneMinutes = 60, timeZoneSummerOnly = False, timeZoneName = "CET" },
                TimeZone { timeZoneMinutes = 120, timeZoneSummerOnly = True, timeZoneName = "CET-DST" }
                ]
        }
getTimeLocale _ mode = defaultTimeLocale {
        dateTimeFmt = "%a %e %b %Y %H:%M" ++ wantSeconds mode
        }

getTimeAndDate :: HC.HostConfiguration -> IO String
getTimeAndDate conf =
        let     localename = HC.locale conf
                locale = getTimeLocale localename conf
        in
                formatTime locale (dateTimeFmt locale) <$> getZonedTime
