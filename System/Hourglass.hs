-- |
-- Module      : System.Hourglass
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Get the system timezone and current time value in multiple formats
--
module System.Hourglass
    (
    -- * Current time in computer friendly format
      timeCurrent
    , timeCurrentP
    -- * Current time in human friendly DateTime format
    , dateCurrent
    , localDateCurrent
    , localDateCurrentAt
    -- * System timezone
    , timezoneCurrent
    ) where

import Control.Applicative
import Data.Hourglass.Types
import Data.Hourglass.Time
import Data.Hourglass.Local
import Data.Hourglass.Internal (systemGetElapsedP, systemGetElapsed, systemGetTimezone)

-- | Get the current elapsed seconds since epoch
timeCurrent :: IO Elapsed
timeCurrent = systemGetElapsed

-- | Get the current elapsed seconds (precise to the nanosecond) since epoch
timeCurrentP :: IO ElapsedP
timeCurrentP = systemGetElapsedP

-- | Get the current global date
--
-- This is equivalent to:
--
-- > timeGetDateTimeOfDay `fmap` timeCurrentP
dateCurrent :: IO DateTime
dateCurrent = timeGetDateTimeOfDay <$> timeCurrentP

-- | Get the localized date by using 'timezoneCurrent' and 'dateCurrent'
localDateCurrent :: IO (LocalTime DateTime)
localDateCurrent = localTime <$> timezoneCurrent <*> dateCurrent

-- | Get the localized date at a specific timezone offset.
localDateCurrentAt :: TimezoneOffset -> IO (LocalTime DateTime)
localDateCurrentAt tz = localTime tz <$> dateCurrent

-- | Get the current timezone offset
--
-- This include daylight saving time when in operation.
timezoneCurrent :: IO TimezoneOffset
timezoneCurrent = systemGetTimezone
