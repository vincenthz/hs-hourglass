-- |
-- Module      : Data.Hourglass.Internal.Win
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Time lowlevel helpers binding to Windows
--
{-# LANGUAGE CPP #-}
module Data.Hourglass.Internal.Win
    ( dateTimeFromUnixEpochP
    , dateTimeFromUnixEpoch
    , systemGetTimezone
    , systemGetElapsed
    , systemGetElapsedP
    ) where

import System.IO.Unsafe
import System.Win32.Time
import Data.Hourglass.Types
import Data.Int (Int64)

unixDiff :: Int64
unixDiff = 11644473600

toFileTime :: Elapsed -> FILETIME
toFileTime (Elapsed (Seconds s)) = FILETIME val
  where val = fromIntegral (s + unixDiff) * 10000000

toElapsedP :: FILETIME -> ElapsedP
toElapsedP (FILETIME w) = ElapsedP (Elapsed $ Seconds s) (NanoSeconds ns)
  where (sWin, hundredNs) = w `divMod` 10000000
        ns = fromIntegral (hundredNs * 100)
        s = fromIntegral sWin - unixDiff

toElapsed :: FILETIME -> Elapsed
toElapsed (FILETIME w) = Elapsed (Seconds s)
  where s = fromIntegral (fst (w `divMod` 10000000)) - unixDiff

callSystemTime :: Elapsed -> SYSTEMTIME
callSystemTime e = unsafePerformIO (fileTimeToSystemTime (toFileTime e))
{-# NOINLINE callSystemTime #-}

dateTimeFromUnixEpochP :: ElapsedP -> DateTime
dateTimeFromUnixEpochP (ElapsedP e ns) = toDateTime $ callSystemTime e
  where toDateTime (SYSTEMTIME wY wM _ wD wH wMin wS _) =
            DateTime (Date (fi wY) (toEnum $ fi $ wM - 1) (fi wD))
                     (TimeOfDay (fi wH) (fi wMin) (fi wS) ns)
        fi :: (Integral a, Num b) => a -> b
        fi = fromIntegral 

dateTimeFromUnixEpoch :: Elapsed -> DateTime
dateTimeFromUnixEpoch e = toDateTime $ callSystemTime e
  where toDateTime (SYSTEMTIME wY wM _ wD wH wMin wS _) =
            DateTime (Date (fi wY) (toEnum $ fi $ wM - 1) (fi wD))
                     (TimeOfDay (fi wH) (fi wMin) (fi wS) 0)
        fi :: (Integral a, Num b) => a -> b
        fi = fromIntegral

systemGetTimezone :: IO TimezoneOffset
systemGetTimezone = do
    (_,tzInfo) <- getTimeZoneInformation
    return $ TimezoneOffset $ getTzOffset tzInfo
  where getTzOffset tzInfo = fromIntegral (tziBias tzInfo - tziDaylightBias tzInfo)

systemGetElapsedP :: IO ElapsedP
systemGetElapsedP = toElapsedP `fmap` getSystemTimeAsFileTime

systemGetElapsed :: IO Elapsed
systemGetElapsed = toElapsed `fmap` getSystemTimeAsFileTime
