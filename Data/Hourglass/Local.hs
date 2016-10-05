-- |
-- Module      : Data.Hourglass.Local
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Local time = global time + timezone
--
{-# LANGUAGE FlexibleInstances #-}
module Data.Hourglass.Local
    (
    -- * Local time
    -- ** Local time type
      LocalTime
    -- ** Local time creation and manipulation
    , localTime
    , localTimeUnwrap
    , localTimeToGlobal
    , localTimeFromGlobal
    , localTimeGetTimezone
    , localTimeSetTimezone
    , localTimeConvert
    ) where

import Data.Hourglass.Types
import Data.Hourglass.Time
import Data.Hourglass.Diff

-- | Local time representation
--
-- this is a time representation augmented by a timezone
-- to get back to a global time, the timezoneOffset needed to be added to the local time.
--
data LocalTime t = LocalTime
    { localTimeUnwrap      :: t              -- ^ unwrap the LocalTime value. the time value is local.
    , localTimeGetTimezone :: TimezoneOffset -- ^ get the timezone associated with LocalTime
    }

-- FIXME add instance Read too.

instance Show t => Show (LocalTime t) where
    show (LocalTime t tz) = show t ++ show tz
instance Eq t => Eq (LocalTime t) where
    LocalTime t1 tz1 == LocalTime t2 tz2 = tz1 == tz2 && t1 == t2

instance (Ord t, Time t) => Ord (LocalTime t) where
    compare l1@(LocalTime g1 tz1) l2@(LocalTime g2 tz2) =
        case compare tz1 tz2 of
            EQ -> compare g1 g2
            _  -> let t1 = localTimeToGlobal l1
                      t2 = localTimeToGlobal l2
                   in compare t1 t2

instance Functor LocalTime where
    fmap f (LocalTime t tz) = LocalTime (f t) tz
instance Time t => Timeable (LocalTime t) where
    timeGetElapsedP    = timeGetElapsedP    . localTimeToGlobal
    timeGetElapsed     = timeGetElapsed     . localTimeToGlobal
    timeGetNanoSeconds = timeGetNanoSeconds . localTimeToGlobal

-- | Create a local time type from a timezone and a time type.
--
-- The time value is assumed to be local to the timezone offset set,
-- so no transformation is done.
localTime :: Time t => TimezoneOffset -> t -> LocalTime t
localTime tz t = LocalTime t tz

-- | Get back a global time value
localTimeToGlobal :: Time t => LocalTime t -> t
localTimeToGlobal (LocalTime local tz)
    | tz == TimezoneOffset 0 = local
    | otherwise              = timeConvert $ elapsedTimeAddSecondsP (timeGetElapsedP local) tzSecs
  where tzSecs = negate $ timezoneOffsetToSeconds tz

-- | create a local time value from a global one
localTimeFromGlobal :: Time t => t -> LocalTime t
localTimeFromGlobal = localTime (TimezoneOffset 0)

-- | Change the timezone, and adjust the local value to represent the new local value.
localTimeSetTimezone :: Time t => TimezoneOffset -> LocalTime t -> LocalTime t
localTimeSetTimezone tz currentLocal@(LocalTime t currentTz)
    | diffTz == 0 = currentLocal
    | otherwise   = LocalTime (timeConvert t') tz
  where t'        = elapsedTimeAddSecondsP (timeGetElapsedP t) diffTz
        diffTz    = timezoneOffsetToSeconds tz - timezoneOffsetToSeconds currentTz

-- | convert the local time representation to another time representation determined by context.
localTimeConvert :: (Time t1, Time t2) => LocalTime t1 -> LocalTime t2
localTimeConvert = fmap timeConvert
