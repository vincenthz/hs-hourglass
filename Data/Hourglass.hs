-- |
-- Module      : Data.Hourglass
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Types and methods for time manipulation.
--
-- The most basic type for time representation is Elapsed, which
-- represent a number of elapsed seconds since the unix epoch.
--
-- Every other defined types can be convert to and from Elapsed type:
--
-- > timeGetElapsed (Date 1 2 3) :: Elapsed
-- > timeFromElapsed 123         :: DateTime
--
-- Local time is represented by any other time types (Elapsed, Date, DateTime, ..),
-- but augmented by a Timezone offset in minutes.
--
-- > localTime (Date 2014 May 4) 600 -- local time at UTC+10 of May 4th 2014
--
module Data.Hourglass
    ( module Data.Hourglass.Time
    , module Data.Hourglass.Types
    , module Data.Hourglass.Format
    , module Data.Hourglass.Local
    , module Data.Hourglass.Zone
    -- * Calendar misc functions
    , isLeapYear
    , getWeekDay
    , getDayOfTheYear
    , daysInMonth
    ) where

import Data.Hourglass.Time
import Data.Hourglass.Format
import Data.Hourglass.Types
import Data.Hourglass.Local
import Data.Hourglass.Zone
import Data.Hourglass.Calendar (isLeapYear, getWeekDay, getDayOfTheYear, daysInMonth)
