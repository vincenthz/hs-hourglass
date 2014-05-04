{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      : Data.Hourglass.Local
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Local time = global time + timezone
--
module Data.Hourglass.Local
    (
    -- * Local time
    -- ** Local time type
      LocalTime(..)
    -- ** Local time creation and manipulation
    , localTime
    , localTimeToGlobal
    , localTimeSetTimezone
    , localTimeConvert
    ) where

import Data.Hourglass.Types
import Data.Hourglass.Time
import Data.Hourglass.Diff

-- | Local time representation
--
-- this is a loca time representation augmented by a timezone
-- to get back to a global time, the timezoneOffset needed to be added to the local time.
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
    timeGetElapsedP (LocalTime t _)    = timeGetElapsedP t
    timeGetElapsed  (LocalTime t _)    = timeGetElapsed t
    timeGetTimezone (LocalTime _ tz)   = Just tz
    timeGetNanoSeconds (LocalTime t _) = timeGetNanoSeconds t

-- | Create a local time type from a timezone and a time type.
--
-- the time value is converted to represent local time
localTime :: Time t => TimezoneOffset -> t -> LocalTime t
localTime tz t = LocalTime (timeConvert t') tz
  where currentTz = maybe (TimezoneOffset 0) id $ timeGetTimezone t
        t'        = elapsedTimeAddSecondsP (timeGetElapsedP t) diffTz
        diffTz    = timezoneOffsetToSeconds tz - timezoneOffsetToSeconds currentTz

-- | Get back a global time value
localTimeToGlobal :: Time t => LocalTime t -> t
localTimeToGlobal (LocalTime local tz) = timeConvert $ elapsedTimeAddSecondsP (timeGetElapsedP local) tzSecs
  where tzSecs = negate $ timezoneOffsetToSeconds tz

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
