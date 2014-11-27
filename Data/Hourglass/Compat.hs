-- |
-- Module      : Data.Hourglass.Compat
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
--
-- Basic Time conversion compatibility.
--
-- This module aims to help conversion between the types from the package
-- time to the package hourglass.
--
-- Example of use (extracted from file Example/Time/Compat.hs):
--
-- > import Data.Hourglass        as H
-- > import Data.Hourglass.Compat as C
-- > import Data.Time             as T
-- >
-- > transpose :: T.ZonedTime
-- >           -> H.LocalTime H.DateTime
-- > transpose oldTime =
-- >     H.localTime
-- >         offsetTime
-- >         (H.DateTime newDate timeofday)
-- >   where
-- >     newDate :: H.Date
-- >     newDate = C.dateFromTAIEpoch $ T.toModifiedJulianDay $ T.localDay $ T.zonedTimeToLocalTime oldTime
-- >
-- >     timeofday :: H.TimeOfDay
-- >     timeofday = C.diffTimeToTimeOfDay $ T.timeOfDayToTime $ T.localTimeOfDay $ T.zonedTimeToLocalTime oldTime
-- >
-- >     offsetTime = H.TimezoneOffset $ fromIntegral $ T.timeZoneMinutes $ T.zonedTimeZone oldTime
--
module Data.Hourglass.Compat
    ( dateFromPOSIXEpoch
    , dateFromTAIEpoch
    , diffTimeToTimeOfDay
    ) where

import Data.Hourglass

-- | Convert an integer which represent the Number of days (To/From) POSIX Epoch
-- to a Date (POSIX Epoch is 1970-01-01).
dateFromPOSIXEpoch :: Integer -- ^ number of days since POSIX Epoch
                   -> Date
dateFromPOSIXEpoch day = do
    let sec = Elapsed $ fromIntegral $ day * 86400
    timeConvert sec

-- | Number of days between POSIX Epoch and TAI Epoch
-- (between 1858-11-17 and 1970-01-01)
daysTAItoPOSIX :: Integer
daysTAItoPOSIX = 40587

-- | Convert an integer which represents the Number of days (To/From) TAI Epoch
-- This function allows use of the package time to easily convert the Day into
-- the Hourglass Date representation (TAI Epoch is 1858-11-17).
-- 
-- This function allows user to easily convert a Data.Time.Calendar.Day into Date
--
-- > import qualified Data.Time.Calendar as T
-- >
-- > timeDay :: T.Day
-- >
-- > dateFromTAIEpoch $ T.toModifiedJulianDay timeDay
dateFromTAIEpoch :: Integer -- ^ number of days since TAI Epoch
                 -> Date
dateFromTAIEpoch dtai =
    dateFromPOSIXEpoch (dtai - daysTAItoPOSIX)

-- | Convert of differential of time of a day.
-- (it convers a Data.Time.Clock.DiffTime into a TimeOfDay)
--
-- Example with DiffTime type from time:
--
-- > import qualified Data.Time.Clock as T
-- >
-- > difftime :: T.DiffTime
-- >
-- > diffTimeToTimeOfDay difftime
--
-- Example with the TimeOfDay type from time:
--
-- > import qualified Data.Time.Clock as T
-- >
-- > timeofday :: T.TimeOfDay
-- >
-- > diffTimeToTimeOfDay $ T.timeOfDayToTime timeofday
diffTimeToTimeOfDay :: Real t
                    => t         -- ^ number of seconds of the time of the day
                    -> TimeOfDay
diffTimeToTimeOfDay dt = do
    let r = toRational dt
    let (secs, nR) = properFraction r :: (Integer, Rational)
    let nsecs = round (nR * 1000000000) :: Integer
    let (minsofday, seconds) = secs `divMod` 60
    let (hours, minutes) = minsofday `divMod` 60
    TimeOfDay
        { todHour = fromIntegral hours
        , todMin  = fromIntegral minutes
        , todSec  = fromIntegral seconds
        , todNSec = fromIntegral nsecs
        }
