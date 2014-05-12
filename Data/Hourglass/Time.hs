-- |
-- Module      : Data.Hourglass.Time
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- generic time representation interface to allow
-- arbitrary conversion between different time representation
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Hourglass.Time
    (
    -- * Generic time classes
      Time(..)
    , Timeable(..)

    -- * Elapsed time
    , Elapsed(..)
    , ElapsedP(..)

    -- * Generic conversion
    , timeConvert

    -- * Date and Time
    , timeGetDate
    , timeGetDateTimeOfDay
    , timeGetTimeOfDay

    -- * Arithmetic
    , TimeDiff(..)
    , timeAdd
    , timeDiff
    , timeDiffP
    ) where

import Data.Data ()
import Data.Hourglass.Types
import Data.Hourglass.Calendar
import Data.Hourglass.Diff
import Foreign.C.Types (CTime(..))

-- | Timeable represent every type that can be made to look like time types.
--
-- * can be converted to ElapsedP and Elapsed
--
-- * optionally have a timezone associated
--
-- * have nanoseconds accessor (which can return 0 when the type is not more precise than seconds)
--
class Timeable t where
    -- | convert a time representation to the number of elapsed seconds and nanoseconds to a specific epoch
    timeGetElapsedP :: t -> ElapsedP

    -- | convert a time representation to the number of elapsed seconds to a specific epoch.
    -- 
    -- defaults to timeGetElapsedP unless defined explicitely by an instance
    timeGetElapsed :: t -> Elapsed
    timeGetElapsed t = e where ElapsedP e _ = timeGetElapsedP t

    -- | return the number of optional nanoseconds.
    --
    -- If the underlaying type is not precise enough to record nanoseconds
    -- (or any variant between seconds and nanoseconds), 0 should be returned
    --
    -- defaults to 'timeGetElapsedP' unless defined explicitely by an instance,
    -- for efficiency reason, it's a good idea to override this methods if
    -- you know the type is not more precise than Seconds.
    timeGetNanoSeconds :: t -> NanoSeconds
    timeGetNanoSeconds t = ns where ElapsedP _ ns = timeGetElapsedP t

-- | Represent time types that can be created from other time types.
--
-- Every conversion happens throught ElapsedP or Elapsed types.
class Timeable t => Time t where
    -- | convert from a number of elapsed seconds and nanoseconds to another time representation
    timeFromElapsedP :: ElapsedP -> t

    -- | convert from a number of elapsed seconds and nanoseconds to another time representation
    --
    -- defaults to timeFromElapsedP unless defined explicitely by an instance.
    timeFromElapsed :: Elapsed -> t
    timeFromElapsed e = timeFromElapsedP (ElapsedP e 0)

instance Timeable CTime where
    timeGetElapsedP c         = ElapsedP (timeGetElapsed c) 0
    timeGetElapsed  (CTime c) = Elapsed (Seconds $ fromIntegral c)
    timeGetNanoSeconds _ = 0
instance Time CTime where
    timeFromElapsedP (ElapsedP e _)       = timeFromElapsed e
    timeFromElapsed (Elapsed (Seconds c)) = CTime (fromIntegral c)

instance Timeable Elapsed where
    timeGetElapsedP  e = ElapsedP e 0
    timeGetElapsed   e = e
    timeGetNanoSeconds _ = 0
instance Time Elapsed where
    timeFromElapsedP (ElapsedP e _) = e
    timeFromElapsed  e = e

instance Timeable ElapsedP where
    timeGetElapsedP    e               = e
    timeGetNanoSeconds (ElapsedP _ ns) = ns
instance Time ElapsedP where
    timeFromElapsedP   e               = e

instance Timeable Date where
    timeGetElapsedP d  = timeGetElapsedP (DateTime d (TimeOfDay 0 0 0 0))
instance Time Date where
    timeFromElapsedP (ElapsedP elapsed _) = d
      where (DateTime d _) = dateTimeFromUnixEpoch elapsed

instance Timeable DateTime where
    timeGetElapsedP d = ElapsedP (dateTimeToUnixEpoch d) (timeGetNanoSeconds d)
    timeGetElapsed d  = dateTimeToUnixEpoch d
    timeGetNanoSeconds (DateTime _ (TimeOfDay _ _ _ ns)) = ns
instance Time DateTime where
    timeFromElapsedP elapsed = dateTimeFromUnixEpochP elapsed

-- | Convert one time representation into another one
-- 
-- The return type need to be infer by the context.
--
-- If the context cannot be infer through this, some specialized functions
-- are available for built-in types:
--
-- * 'timeGetDate'
--
-- * 'timeGetDateTimeOfDay'
--
-- * 'timeGetElapsed', 'timeGetElapsedP'
timeConvert :: (Timeable t1, Time t2) => t1 -> t2
timeConvert t1 = timeFromElapsedP (timeGetElapsedP t1)
{-# INLINE[2] timeConvert #-}
{-# RULES "timeConvert/ID" timeConvert = id #-}
{-# RULES "timeConvert/ElapsedP" timeConvert = timeGetElapsedP #-}
{-# RULES "timeConvert/Elapsed" timeConvert = timeGetElapsed #-}

-- | Get the calendar Date (year-month-day) from a time representation
--
-- specialization of 'timeConvert'
timeGetDate :: Timeable t => t -> Date
timeGetDate t = d where (DateTime d _) = timeGetDateTimeOfDay t
{-# INLINE[2] timeGetDate #-}
{-# RULES "timeGetDate/ID" timeGetDate = id #-}
{-# RULES "timeGetDate/DateTime" timeGetDate = dtDate #-}

-- | Get the day time (hours:minutes:seconds) from a time representation
--
-- specialization of 'timeConvert'
timeGetTimeOfDay :: Timeable t => t -> TimeOfDay
timeGetTimeOfDay t = tod where (DateTime _ tod) = timeGetDateTimeOfDay t
{-# INLINE[2] timeGetTimeOfDay #-}
{-# RULES "timeGetTimeOfDay/Date" timeGetTimeOfDay = const (TimeOfDay 0 0 0 0) #-}
{-# RULES "timeGetTimeOfDay/DateTime" timeGetTimeOfDay = dtTime #-}

-- | Get the date and time of day from a time representation
--
-- specialization of 'timeConvert'
timeGetDateTimeOfDay :: Timeable t => t -> DateTime
timeGetDateTimeOfDay t = dateTimeFromUnixEpochP $ timeGetElapsedP t
{-# INLINE[2] timeGetDateTimeOfDay #-}
{-# RULES "timeGetDateTimeOfDay/ID" timeGetDateTimeOfDay = id #-}
{-# RULES "timeGetDateTimeOfDay/Date" timeGetDateTimeOfDay = flip DateTime (TimeOfDay 0 0 0 0) #-}

-- | add some values with time units to a time representation and returns this new time representation
--
-- example:
--
-- > t1 `timeAdd` mempty { timeDiffHours = 12 }
timeAdd :: Time t => t -> TimeDiff -> t
timeAdd t td = timeFromElapsedP $ elapsedTimeAddP (timeGetElapsedP t) td

-- | Get the difference in seconds between two time representation
--
-- effectively:
--
-- > t2 `timeDiff` t1 = t2 - t1
timeDiff :: (Timeable t1, Timeable t2) => t1 -> t2 -> Seconds
timeDiff t1 t2 = sec where (Elapsed sec) = timeGetElapsed t1 - timeGetElapsed t2

-- | Get the difference in seconds and nanoseconds between two time representation
--
-- effectively:
--
-- > @t2 `timeDiffP` t1 = t2 - t1
timeDiffP :: (Timeable t1, Timeable t2) => t1 -> t2 -> (Seconds, NanoSeconds)
timeDiffP t1 t2 = (sec, ns)
  where (ElapsedP (Elapsed sec) ns) = timeGetElapsedP t1 - timeGetElapsedP t2
