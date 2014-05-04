-- |
-- Module      : Data.Hourglass.Calendar
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Misc calendar functions
--
module Data.Hourglass.Calendar
    ( isLeapYear
    , getWeekDay
    , getDayOfTheYear
    , dateToUnixEpoch
    , dateFromUnixEpoch
    , todToSeconds
    , dateTimeToUnixEpoch
    , dateTimeFromUnixEpoch
    , dateTimeFromUnixEpochP
    ) where

import Data.Hourglass.Types
import Data.Hourglass.Internal

-- | Return if this year is a leap year (366 days)
-- or not (365 days in a year)
isLeapYear :: Int -> Bool
isLeapYear year
    | year `mod` 4 /= 0   = False
    | year `mod` 100 /= 0 = True
    | year `mod` 400 == 0 = True
    | otherwise           = False

-- | Return the day of the week a specific date fall in
getWeekDay :: Date -> WeekDay
getWeekDay date = toEnum (d `mod` 7)
  where d = daysOfDate date

-- | return the number of days until the beggining of the month specified for a specific year.
daysUntilMonth :: Int -> Month -> Int
daysUntilMonth y m
    | isLeapYear y = leapYears !! fromEnum m
    | otherwise    = normalYears !! fromEnum m
  where normalYears = [ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ]
        leapYears   = [ 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 ]

{-
-- | Return the number of days in a month.
daysInMonth :: Int -> Month -> Int
daysInMonth y m
    | m == February && isLeapYear y = 29
    | otherwise                     = days !! fromEnum m
  where days = [31,28,31,30,31,30,31,31,30,31,30,31]
-}

-- | return the day of the year where Jan 1 is 0
--
-- between 0 and 364. 365 for leap years
getDayOfTheYear :: Date -> Int
getDayOfTheYear (Date y m d) = daysUntilMonth y m + d

-- | return the number of days before Jan 1st of the year
daysBeforeYear :: Int -> Int
daysBeforeYear year = y * 365 + (y `div` 4) - (y `div` 100) + (y `div` 400)
  where y = year - 1

-- | Return the number of day since 1 january 1
daysOfDate :: Date -> Int
daysOfDate (Date y m d) = daysBeforeYear y + daysUntilMonth y m + d

-- | Return the number of seconds to unix epoch of a date considering hour=0,minute=0,second=0
dateToUnixEpoch :: Date -> Elapsed
dateToUnixEpoch date = Elapsed $ Seconds (fromIntegral (daysOfDate date - epochDays) * secondsPerDay)
  where epochDays     = 719163
        secondsPerDay = 86400

-- | Return the Date associated with the unix epoch
dateFromUnixEpoch :: Elapsed -> Date
dateFromUnixEpoch e = dtDate $ dateTimeFromUnixEpoch e

-- | Return the number of seconds from a time structure
todToSeconds :: TimeOfDay -> Seconds
todToSeconds (TimeOfDay h m s _) =
    fromIntegral h * 3600 + fromIntegral m * 60 + fromIntegral s

-- | Return the number of seconds to unix epoch of a date time
dateTimeToUnixEpoch :: DateTime -> Elapsed
dateTimeToUnixEpoch (DateTime d t) = dateToUnixEpoch d + Elapsed (todToSeconds t)
