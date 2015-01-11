-- |
-- Module      : Data.Hourglass.Zone
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Timezone utility
--
{-# LANGUAGE ExistentialQuantification #-}
module Data.Hourglass.Zone
    ( Timezone(..)
    , UTC(..)
    , TimezoneMinutes(..)
    ) where

-- | standard representation for timezone
class Timezone tz where
    -- | offset in minutes from UTC. valid values should be between -12*60 to +14*60
    timezoneOffset :: tz -> Int
    -- | the name of the timezone. by default will be +-HH:MM encoding.
    timezoneName :: tz -> String
    timezoneName = tzMinutesPrint . timezoneOffset

-- | Simple timezone containing the number of minutes difference
-- with UTC.
--
-- Valid values should be between -12*60 to +14*60
newtype TimezoneMinutes = TimezoneMinutes Int
    deriving (Show,Eq,Ord)

-- | Universal Time Coordinated. The generic computer "timezone".
data UTC = UTC
    deriving (Show,Eq,Ord)

instance Timezone UTC where
    timezoneOffset _ = 0
    timezoneName _   = "UTC"

instance Timezone TimezoneMinutes where
    timezoneOffset (TimezoneMinutes minutes) = minutes

-- | print a minute offset in format:
-- (+-)HH:MM
tzMinutesPrint :: Int -> String
tzMinutesPrint offset =
      (if offset > 0 then '+' else '-')
    : (pad0 h ++ ":" ++ pad0 m)
  where (h,m)  = abs offset `divMod` 60
        pad0 v
            | v < 10    = '0':show v
            | otherwise = show v
