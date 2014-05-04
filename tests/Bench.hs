{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Criterion.Main
import Data.Hourglass
import TimeDB

import Data.List (intercalate)
import qualified Data.Time.Calendar as T
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.POSIX as T
import qualified System.Locale as T

timeToTuple :: T.UTCTime -> (Int, Int, Int, Int, Int, Int)
timeToTuple utcTime = (fromIntegral y, m, d, h, mi, sec)
 where (y,m,d) = T.toGregorian (T.utctDay utcTime)
       daytime    = floor $ T.utctDayTime utcTime
       (dt, sec)= daytime `divMod` 60
       (h , mi) = dt `divMod` 60
       --(DateTimeP (Date y m d) (TimeOfDayP (TimeOfDay h mi sec) _)) = localTimeToGlobal localtime

elapsedToPosixTime :: Elapsed -> T.POSIXTime
elapsedToPosixTime (Elapsed (Seconds s)) = fromIntegral s

timePosixDict :: [ (Elapsed, T.POSIXTime) ]
timePosixDict =
    [ (Elapsed 0, 0)
    , (Elapsed 1000000, 1000000)
    , (Elapsed 9000099, 9000099)
    , (Elapsed 1398232846, 1398232846) -- currentish time (at the time of writing)
    , (Elapsed 5134000099, 5134000099)
    , (Elapsed 10000000000000, 10000000000000) -- year 318857 ..
    ]

dateDict :: [ (Int, Int, Int, Int, Int, Int) ]
dateDict =
    [ (1970, 1, 1, 1, 1, 1)
    , (2014, 5, 5, 5, 5, 5)
    , (2114, 11, 5, 5, 5, 5)
    ]

-- T.posixSecondsToUTCTime timePosix

main = defaultMain
    [ bgroup "highlevel" $ concatMap toHighLevel timePosixDict
    , bgroup "to-calendar"  $ concatMap toCalendar timePosixDict
    , bgroup "to-posix" $ concatMap toPosix dateDict
    ]
  where toHighLevel (posixHourglass, posixTime) =
            [ bench (show $ posixHourglass) $ nf timeGetDateTimeOfDay posixHourglass
            , bench (show $ posixTime) $ nf T.posixSecondsToUTCTime posixTime
            ]
        toCalendar (posixHourglass, posixTime) =
            [ bench (show $ posixHourglass) $ nf timeGetDateTimeOfDay posixHourglass
            , bench (show $ posixTime) $ nf (timeToTuple . T.posixSecondsToUTCTime) posixTime
            ]
        toPosix v =
            [ bench (n v) $ nf hourglass v
            , bench (n v) $ nf time v
            ]
          where n (y,m,d,h,mi,s) = (intercalate "-" $ map show [y,m,d]) ++ " " ++ (intercalate ":" $ map show [h,mi,s])
                hourglass (y,m,d,h,mi,s) = timeGetElapsed $ DateTime (Date y (toEnum (m-1)) d) (TimeOfDay h mi s 0)
                time      (y,m,d,h,mi,s) = let day = T.fromGregorian (fromIntegral y) m d
                                               diffTime = T.secondsToDiffTime $ fromIntegral (h * 3600 + mi * 60 + s)
                                            in T.utcTimeToPOSIXSeconds (T.UTCTime day diffTime)

            --bench (show $ fromIntegral posixHourglass) $ nf (
