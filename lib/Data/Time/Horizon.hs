{-# LANGUAGE ViewPatterns #-}

{-
  Module     : Data.Time.Horizon
  License    : BSD3
  Copyright  : (C) 2015 Joel Stanley
  Maintainer : Joel Stanley <intractable@gmail.com>
  Stability  : provisional
  
  Provides approximate sunrise and sunset times in UTC from latitude and 
  longitude coordinates.

  See https://en.wikipedia.org/wiki/Sunrise_equation.
  See http://aa.usno.navy.mil/data/docs/RS_OneYear.php.

-}

module Data.Time.Horizon
  ( LatitudeNorth
  , LongitudeWest
  , sunrise
  , sunset
  )
  where

import Data.Angle
import Data.Fixed
import Data.Time

type LatitudeNorth = Double 
type LongitudeWest = Double

-- | Returns an approximated UTC time of the sunrise on the given UTC day at the given location.
sunrise :: Day -> LongitudeWest -> LatitudeNorth -> UTCTime
sunrise d lw ln = mkUTC d . jdToSeconds $ sunrise' d lw ln

-- | Returns an approximated UTC time of the sunset on the given UTC day at the given location.
sunset :: Day -> LongitudeWest -> LatitudeNorth -> UTCTime
sunset d lw ln = mkUTC d .  jdToSeconds $ sunset' d lw ln

-- | Approximate the Julian date of the sunrise on the given UTC day at the given location.
sunrise' :: Day -> LongitudeWest -> LatitudeNorth -> Double
sunrise' d lw ln = jtransit - (jset - jtransit)
  where
    u        = mkUTC d 0
    jtransit = solarTransit u lw
    jset     = sunset' d lw ln

-- | Approximate the Julian date of the sunset on the given UTC day at the given location.
sunset' :: Day -> LongitudeWest -> LatitudeNorth -> Double
sunset' d lw ln =
  2451545.0009 + ((w0 + lw) / 360) + n + 0.0053 * sine m - 0.0069 * sine (2 * lambda)
  where
    u          = mkUTC d 0
    Degrees w0 = omega0 u lw ln
    n          = fromIntegral (julianCycle u lw :: Integer)
    m          = solarMeanAnomaly u lw
    lambda     = eclipticLongitude u lw

toJD :: RealFrac a => UTCTime -> a
toJD = (+2400000.5) . toMJD
  where
    toMJD (UTCTime (fromIntegral . toModifiedJulianDay -> d) (toRational -> dt)) =
      fromRational (d + dt / 86401)

jdToSeconds :: (RealFrac a, Integral b) => a -> b
jdToSeconds jd = floor (dayFrac * 86401)
  where
    dayFrac = mjd - fromIntegral (floor mjd :: Integer)
    mjd     = jd - 2400000.5

julianCycle :: Integral a => UTCTime -> LongitudeWest -> a
julianCycle u lw = n
  where
    n     = floor (nstar + 0.5)
    nstar = jdate - 2451545.0009 - (lw / 360)
    jdate = toJD u

approxSolarNoon :: UTCTime -> LongitudeWest -> Double
approxSolarNoon u lw = jstar
  where
    jstar = 2451545.0009 + lw / 360 + fromIntegral (julianCycle u lw :: Int)

solarMeanAnomaly :: UTCTime -> LongitudeWest -> Degrees Double
solarMeanAnomaly u lw = Degrees m
  where
    m = (357.5291 + 0.98560028 * (approxSolarNoon u lw - 2451545)) `mod'` 360

equationOfCenter :: UTCTime -> LongitudeWest -> Double
equationOfCenter u lw = c
  where
    c = 1.9148 * sine m + 0.0200 * sine (2 * m) + 0.0003 * sine (3 * m)
    m = solarMeanAnomaly u lw

eclipticLongitude :: UTCTime -> LongitudeWest -> Degrees Double
eclipticLongitude u lw = Degrees lambda
  where
    lambda = (m + 102.9372 + c + 180) `mod'` 360
    Degrees m = solarMeanAnomaly u lw
    c         = equationOfCenter u lw

solarTransit :: UTCTime -> LongitudeWest -> Double
solarTransit u lw = jtransit
  where
    jtransit = jstar + 0.0053 * sine m - 0.0069 * sine (2 * lambda)
    jstar    = approxSolarNoon u lw
    m        = solarMeanAnomaly u lw
    lambda   = eclipticLongitude u lw

declination :: UTCTime -> LongitudeWest -> Degrees Double
declination u lw = arcsine (sine lambda * sine (Degrees 23.45))
  where
    lambda = eclipticLongitude u lw

omega0 :: UTCTime -> LongitudeWest -> LatitudeNorth -> Degrees Double
omega0 u lw ln = arccosine (num / denom)
  where
    num    = sine (Degrees (-0.83)) - sine phi * sine gamma'
    denom  = cosine phi * cosine gamma'
    phi    = Degrees ln
    gamma' = declination u lw

mkUTC :: Day -> Integer -> UTCTime
mkUTC d = UTCTime d . secondsToDiffTime
