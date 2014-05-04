-- |
-- Module      : Data.Hourglass.Diff
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- time arithmetic methods
--
module Data.Hourglass.Diff
    ( TimeDiff(..)
    , normalizeTimeDiff
    , dateTimeAdd
    , elapsedTimeAddSeconds
    , elapsedTimeAddSecondsP
    , elapsedTimeAdd
    , elapsedTimeAddP
    ) where

import Data.Int
import Data.Monoid
import Data.Hourglass.Types
import Data.Hourglass.Calendar

-- | A simple Time difference structure.
--
-- A null time difference can be created with 'mempty' and
-- the structures can be combined with 'mappend' or 'mconcat'.
--
-- Example:
--
-- > mempty { timeDiffMonths = 1 } `mappend` mempty { timeDiffSeconds = 24 }
--
data TimeDiff = TimeDiff
    { timeDiffYears   :: Int -- ^ number of years
    , timeDiffMonths  :: Int -- ^ number of months
    , timeDiffDays    :: Int -- ^ number of days
    , timeDiffHours   :: Int -- ^ number of hours
    , timeDiffMinutes :: Int -- ^ number of minutes
    , timeDiffSeconds :: Int -- ^ number of seconds
    , timeDiffNs      :: Int -- ^ number of nanoseconds
    } deriving (Show,Eq)

-- | simplify a time difference
toSimplerTimeDiff :: TimeDiff -> (Int, Int, Seconds, NanoSeconds)
toSimplerTimeDiff (TimeDiff y m d h mi s ns) =
    (y, m, Seconds accSecs, NanoSeconds ns')
  where accSecs = (((i64 d * 24) + i64 h) * 60 + i64 mi) * 60 + i64 s + i64 sacc
        (sacc, ns') = ns `divMod` 1000000000

        i64 :: Int -> Int64
        i64 = fromIntegral

instance Monoid TimeDiff where
    mempty  = TimeDiff 0 0 0 0 0 0 0
    mappend (TimeDiff f1 f2 f3 f4 f5 f6 f7) (TimeDiff g1 g2 g3 g4 g5 g6 g7) =
        TimeDiff (f1+g1) (f2+g2) (f3+g3) (f4+g4) (f5+g5) (f6+g6) (f7+g7)

-- | Normalize all constant bounded fields,
-- i.e. all except days since we don't know to which
-- months they apply.
normalizeTimeDiff :: TimeDiff -> TimeDiff
normalizeTimeDiff (TimeDiff y m d h mi s ns) =
    TimeDiff y' m' (d+dacc) h' mi' s' ns'
  where
    y'          = y + macc
    (macc, m')  = m `divMod` 12
    (dacc, h')  = (h+hacc) `divMod` 24
    (hacc, mi') = (mi+miacc) `divMod` 60
    (miacc, s') = (s+sacc) `divMod` 60
    (sacc, ns') = ns `divMod` 1000000000

-- | relatively add some years and months to a date
dateTimeAddYM :: DateTime -> (Int, Int) -> DateTime
dateTimeAddYM (DateTime (Date y m d) tod) (yDiff, mDiff) =
    DateTime (Date (y+yDiff+yDiffAcc) (toEnum mNew) d) tod
  where
    (yDiffAcc,mNew) = (fromEnum m + mDiff) `divMod` 12

-- | add to a DateTime
dateTimeAdd :: DateTime -> TimeDiff -> DateTime
dateTimeAdd dt td =
    dateTimeFromUnixEpoch $ elapsedTimeAdd (dateTimeToUnixEpoch dt) td

-- | add a (year,month,seconds) to an Elapsed
elapsedTimeAddSimple :: Elapsed -> (Int, Int, Seconds) -> Elapsed
elapsedTimeAddSimple e (y,m,secs)
    | y == 0 && m == 0 = e'
    | otherwise        =
        let dt = dateTimeFromUnixEpoch e'
         in dateTimeToUnixEpoch $ dateTimeAddYM dt (y, m)
  where e' = e + Elapsed secs

-- | Add a number of seconds to an Elapsed type
elapsedTimeAddSeconds :: Elapsed -> Seconds -> Elapsed
elapsedTimeAddSeconds (Elapsed s1) s2 = Elapsed (s1+s2)

-- | Add a number of seconds to an ElapsedP type
elapsedTimeAddSecondsP :: ElapsedP -> Seconds -> ElapsedP
elapsedTimeAddSecondsP (ElapsedP (Elapsed s1) ns1) s2 = ElapsedP (Elapsed (s1+s2)) ns1

-- | add a time difference
elapsedTimeAdd :: Elapsed -> TimeDiff -> Elapsed
elapsedTimeAdd e td = elapsedTimeAddSimple e (y,m,secs)
  where (y,m,secs,_) = toSimplerTimeDiff td

-- | add a time difference with nanoseconds precisions
elapsedTimeAddP :: ElapsedP -> TimeDiff -> ElapsedP
elapsedTimeAddP (ElapsedP e (NanoSeconds ns)) td = ElapsedP e' ns'
  where (y,m,secs,ns') = toSimplerTimeDiff td'
        e' = elapsedTimeAddSimple e (y,m,secs)
        td' = td { timeDiffNs = timeDiffNs td + ns }

{- disabled for warning purpose. to be implemented

-- | Duration string to time diff
--
-- <http://en.wikipedia.org/wiki/ISO_8601#Durations>
--
-- * P is the duration designator (historically called "period") placed at the start of the duration representation.
--
-- * Y is the year designator that follows the value for the number of years.
--
-- * M is the month designator that follows the value for the number of months.
--
-- * W is the week designator that follows the value for the number of weeks.
--
-- * D is the day designator that follows the value for the number of days.
--
-- * T is the time designator that precedes the time components of the representation.
--
-- * H is the hour designator that follows the value for the number of hours.
--
-- * M is the minute designator that follows the value for the number of minutes.
--
-- * S is the second designator that follows the value for the number of seconds.
--
timeDiffFromDuration :: String -> TimeDiff
timeDiffFromDuration _ = undefined

-- | Human description string to time diff
--
-- examples:
--
-- * "1 day"
--
-- * "2 months, 5 days and 1 second"
--
timeDiffFromDescription :: String -> TimeDiff
timeDiffFromDescription _ = undefined
-}
