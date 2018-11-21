module DateFormatter
        ( getTimeAndDate
        ) where

import Data.Time.Format
import Data.Time.LocalTime

import qualified HostConfiguration as HC

wantSeconds :: HC.SysInfoBarMode -> String
wantSeconds HC.Slim = ""
wantSeconds HC.Full = ":%S"

getTimeLocale :: String -> HC.SysInfoBarMode -> TimeLocale
getTimeLocale "de" mode = TimeLocale {
        wDays = fmap (\x -> (x, take 2 x)) [ "Sonntag", "Montag", "Dienstag",
                "Mittwoch", "Donnerstag", "Freitag", "Samstag" ],
        months = fmap (\x -> (x, take 3 x)) [ "Januar", "Februar", "März",
                "April", "Mai", "Juni", "Juli", "August", "September",
                "Oktober", "November", "Dezember" ],
        amPm = ("früh", "nachm."),
        dateTimeFmt = "%a %e %b %Y %H:%M" ++ wantSeconds mode,
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

getTimeAndDate :: String -> HC.SysInfoBarMode -> IO String
getTimeAndDate localename mode =
        formatTime locale (dateTimeFmt locale) <$> getZonedTime
        where locale = getTimeLocale localename mode
