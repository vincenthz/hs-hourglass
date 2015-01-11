{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Criterion.Main
import Data.Hourglass
import System.Hourglass
import TimeDB

import Data.List (intercalate)
import qualified Data.Time.Calendar as T
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.POSIX as T
import qualified System.Locale as T

timeToTuple :: T.UTCTime -> (Int, Int, Int, Int, Int, Int)
timeToTuple utcTime = (fromIntegral y, m, d, h, mi, sec)
 where (!y,!m,!d)  = T.toGregorian (T.utctDay utcTime)
       !daytime    = floor $ T.utctDayTime utcTime
       (!dt, !sec) = daytime `divMod` 60
       (!h , !mi)  = dt `divMod` 60

timeToTupleDate :: T.UTCTime -> (Int, Int, Int)
timeToTupleDate utcTime = (fromIntegral y, m, d)
  where (!y,!m,!d)  = T.toGregorian (T.utctDay utcTime)

elapsedToPosixTime :: Elapsed -> T.POSIXTime
elapsedToPosixTime (Elapsed (Seconds s)) = fromIntegral s

timePosixDict :: [ (Elapsed, T.POSIXTime) ]
timePosixDict =
    [-- (Elapsed 0, 0)
    --, (Elapsed 1000000, 1000000)
    --, (Elapsed 9000099, 9000099)
    {-,-} (Elapsed 1398232846, 1398232846) -- currentish time (at the time of writing)
    --, (Elapsed 5134000099, 5134000099)
    --, (Elapsed 10000000000000, 10000000000000) -- year 318857 ..
    ]

dateDict :: [ (Int, Int, Int, Int, Int, Int) ]
dateDict =
    [{- (1970, 1, 1, 1, 1, 1)
    , -}(2014, 5, 5, 5, 5, 5)
    --, (2114, 11, 5, 5, 5, 5)
    ]

main :: IO ()
main = defaultMain
    [ bgroup "highlevel"   $ concatMap toHighLevel timePosixDict
    , bgroup "to-dateTime" $ concatMap toCalendar timePosixDict
    , bgroup "to-date"     $ concatMap toCalendarDate timePosixDict
    , bgroup "utc-to-date" $ concatMap toCalendarUTC timePosixDict
    , bgroup "to-posix"    $ concatMap toPosix dateDict
    , bgroup "system"      fromSystem
    ]
  where toHighLevel (posixHourglass, posixTime) =
            [ bench (showH posixHourglass) $ nf timeGetDateTimeOfDay posixHourglass
            , bench (showT posixTime) $ nf T.posixSecondsToUTCTime posixTime
            ]
        toCalendar (posixHourglass, posixTime) =
            [ bench (showH posixHourglass) $ nf timeGetDateTimeOfDay posixHourglass
            , bench (showT posixTime) $ nf (timeToTuple . T.posixSecondsToUTCTime) posixTime
            ]
        toCalendarDate (posixHourglass, posixTime) =
            [ bench (showH posixHourglass) $ nf timeGetDate posixHourglass
            , bench (showT posixTime) $ nf (timeToTupleDate . T.posixSecondsToUTCTime) posixTime
            ]
        toCalendarUTC (posixHourglass, posixTime) = 
            [ bench (showH posixHourglass) $ nf timeGetDateTimeOfDay posixHourglass
            , bench (showT utcTime) $ nf timeToTuple utcTime
            ]
          where !utcTime = T.posixSecondsToUTCTime posixTime
        toPosix v =
            [ bench ("hourglass/" ++ n v) $ nf hourglass v
            , bench ("time/" ++ n v) $ nf time v
            ]
          where n (y,m,d,h,mi,s) = intercalate "-" (map show [y,m,d]) ++ " " ++ intercalate ":" (map show [h,mi,s])
                hourglass (y,m,d,h,mi,s) = timeGetElapsed $ DateTime (Date y (toEnum (m-1)) d) (TimeOfDay (fromIntegral h) (fromIntegral mi) (fromIntegral s) 0)
                time      (y,m,d,h,mi,s) = let day = T.fromGregorian (fromIntegral y) m d
                                               diffTime = T.secondsToDiffTime $ fromIntegral (h * 3600 + mi * 60 + s)
                                            in T.utcTimeToPOSIXSeconds (T.UTCTime day diffTime)

        fromSystem =
            [ bench "hourglass/p"    $ nfIO timeCurrent
            , bench "hourglass/ns"   $ nfIO timeCurrentP
            , bench "time/posixTime" $ nfIO T.getPOSIXTime
            , bench "time/utcTime"   $ nfIO T.getCurrentTime
            ]

        showH :: Show a => a -> String
        showH a = "hourglass/" ++ show a
        showT :: Show a => a -> String
        showT a = "time/" ++ show a
