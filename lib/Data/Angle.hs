{- |
  Deal with angles measured in degrees or radians.

  Names are kept deliberately different from the standard prelude to avoid name clashes.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Angle where

-- | An angle in radians.
newtype Radians x = Radians x deriving (Eq, Ord, Show, Num, Fractional)

-- | An angle in degrees.
newtype Degrees x = Degrees x deriving (Eq, Ord, Show, Num, Fractional)

-- | Convert from radians to degrees.
degrees :: (Floating x) => Radians x -> Degrees x
degrees (Radians x) = Degrees (x/pi*180)

-- | Convert from degrees to radians.
radians :: (Floating x) => Degrees x -> Radians x
radians (Degrees x) = Radians (x/180*pi)

-- | Type-class for angles.
class Angle a where
  sine    :: (Floating x) => a x -> x
  cosine  :: (Floating x) => a x -> x
  tangent :: (Floating x) => a x -> x

  arcsine    :: (Floating x) => x -> a x
  arccosine  :: (Floating x) => x -> a x
  arctangent :: (Floating x) => x -> a x

instance Angle Radians where
  sine    (Radians x) = sin x
  cosine  (Radians x) = cos x
  tangent (Radians x) = tan x

  arcsine    x = Radians (asin x)
  arccosine  x = Radians (acos x)
  arctangent x = Radians (atan x)

instance Angle Degrees where
  sine    =    sine . radians
  cosine  =  cosine . radians
  tangent = tangent . radians

  arcsine    = degrees . arcsine
  arccosine  = degrees . arccosine
  arctangent = degrees . arctangent
