{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Main where

import Control.Applicative
import Data.Monoid (mempty)
--import Control.DeepSeq

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit hiding (Test)
import Test.QuickCheck

import Data.Word
import Data.Int
import Data.Hourglass
import Data.Hourglass.Epoch
--import System.Hourglass

import Foreign.Storable
import Foreign.C.Types (CTime)

import qualified Data.Time.Calendar as T
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.POSIX as T
import qualified Data.Time.Format as T
import qualified System.Locale as T

import qualified Control.Exception as E

import TimeDB

tmPosix0 :: Elapsed
tmPosix0 = fromIntegral (0 :: Word64)

timePosix0 :: T.POSIXTime
timePosix0 = fromIntegral (0 :: Word64)

elapsedToPosixTime :: Elapsed -> T.POSIXTime
elapsedToPosixTime (Elapsed (Seconds s)) = fromIntegral s

dateEqual :: LocalTime DateTime -> T.UTCTime -> Bool
dateEqual localtime utcTime =
    and [ fromIntegral y == y', m' == (fromEnum m + 1), d' == d
        , h' == h, mi' == mi, sec' == sec ]
 where (y',m',d') = T.toGregorian (T.utctDay utcTime)
       daytime    = floor $ T.utctDayTime utcTime
       (dt', sec')= daytime `divMod` 60
       (h' , mi') = dt' `divMod` 60
       (DateTime (Date y m d) (TimeOfDay h mi sec _)) = localTimeToGlobal localtime

-- windows native functions to convert time cannot handle time before year 1601
#ifdef WINDOWS
loElapsed = -11644473600 -- ~ year 1601
hiElapsed =  32503680000
dateRange = (1800, 2202)
#else
isCTime64 = sizeOf (undefined :: CTime) == 8
loElapsed =
  if isCTime64
     then -62135596800 -- ~ year 0
     else -(2^(28 :: Int))
hiElapsed =
  if isCTime64
     then 2^(55 :: Int) -- in a future far far away
     else 2^(29 :: Int) -- before the 2038 bug.
dateRange =
  if isCTime64
     then (1800, 2202)
     else (1960, 2036)
#endif
instance Arbitrary Seconds where
    arbitrary = Seconds . toHiLo <$> arbitrary
      where toHiLo v | v > loElapsed && v < hiElapsed = v
                     | v > hiElapsed = v `mod` hiElapsed
                     | v < loElapsed = v `mod` loElapsed
                     | otherwise = error "internal error"

instance Arbitrary NanoSeconds where
    arbitrary = NanoSeconds <$> choose (0, 100000000)
instance Arbitrary Elapsed where
    arbitrary = Elapsed <$> arbitrary
instance Arbitrary TimezoneOffset where
    arbitrary = TimezoneOffset <$> choose (-11*60,11*60)
instance Arbitrary TimeDiff where
    arbitrary = TimeDiff <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> choose (0, 100000)
instance Arbitrary Month where
    arbitrary = elements [January ..]
instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary <*> arbitrary
instance Arbitrary Date where
    arbitrary = Date <$> choose dateRange
                     <*> arbitrary
                     <*> choose (1,28)
instance Arbitrary TimeOfDay where
    arbitrary = TimeOfDay <$> choose (0,23)
                          <*> choose (0,59)
                          <*> choose (0,59)
                          <*> arbitrary
instance (Time t, Arbitrary t) => Arbitrary (LocalTime t) where
    arbitrary = localTime <$> arbitrary <*> arbitrary

eq expected got
    | expected == got = True
    | otherwise       = error ("expected: " ++ show expected ++ " got: " ++ show got)

tests knowns =
    [ testGroup "known"
        [ testGroup "calendar conv" (map toCalendarTest $ zip [1..] (map tuple12 knowns))
        , testGroup "seconds conv" (map toSecondTest $ zip [1..] (map tuple12 knowns))
        , testGroup "weekday" (map toWeekDayTest $ zip [1..] (map tuple13 knowns))
        ]
    , testGroup "conversion"
        [ testProperty "calendar" $ \(e :: Elapsed) ->
             e `eq` timeGetElapsed (timeGetDateTimeOfDay e)
        , testProperty "win epoch" $ \(e :: Elapsed) ->
            let e2 = timeConvert e :: ElapsedSince WindowsEpoch
             in timePrint ISO8601_DateAndTime e `eq` timePrint ISO8601_DateAndTime e2
        ]
    , testGroup "localtime"
        [ testProperty "eq" $ \(l :: LocalTime Elapsed) ->
            let g = localTimeToGlobal l
             in l `eq` localTime (localTimeGetTimezone l) g
        , testProperty "set" $ \(l :: LocalTime Elapsed, newTz) ->
            let l2 = localTimeSetTimezone newTz l
             in localTimeToGlobal l `eq` localTimeToGlobal l2
        ]
    , testGroup "arithmetic"
        [ testProperty "add-diff" $ \(e :: Elapsed, tdiff) ->
            let d@(TimeDiff _ _ day h mi s _) = tdiff { timeDiffYears  = 0
                                                      , timeDiffMonths = 0
                                                      , timeDiffNs     = 0
                                                      }
                i64     = fromIntegral
                accSecs = (((i64 day * 24) + i64 h) * 60 + i64 mi) * 60 + i64 s :: Int64
                e'      = timeAdd e d
             in Seconds accSecs `eq` timeDiff e' e
        , testProperty "calendar-add-month" $ \date@(DateTime (Date y m d) _) ->
            let date'@(DateTime (Date y' m' d') _) = timeAdd date (mempty { timeDiffMonths = 1 })
             in timeGetTimeOfDay date `eq` timeGetTimeOfDay date' &&
                (d `eq` d')                                       &&
                (toEnum ((fromEnum m+1) `mod` 12) `eq` m')        &&
                (if m == December then (y+1) `eq` y' else y `eq` y')
        ]
    , testGroup "formating"
        [ testProperty "iso8601 date" $ \(e :: Elapsed) ->
            (calTimeFormatTimeISO8601 (elapsedToPosixTime e) `eq` timePrint ISO8601_Date e)
        , testProperty "unix seconds" $ \(e :: Elapsed) ->
            let sTime = T.formatTime T.defaultTimeLocale "%s" (T.posixSecondsToUTCTime $ elapsedToPosixTime e)
                sHg = timePrint "EPOCH" e
             in sTime `eq` sHg
        ]
    , testGroup "parsing"
        [ testProperty "iso8601 date" $ \(e :: Elapsed) ->
            let fmt = calTimeFormatTimeISO8601 (elapsedToPosixTime e)
                ed1  = timeParseE ISO8601_Date fmt
                md2  = T.parseTime T.defaultTimeLocale fmt "%F"
             in case (ed1,md2) of
                    (Left _, Nothing)         -> error ("both cannot parse: " ++ show fmt)
                    (Left err, Just _)        -> error ("error parsing string: " ++ show err)
                    (Right (d1, ""), Just d2) -> dateEqual d1 d2
                    (Right (_,_), Nothing)    -> True -- let (LocalTime tparsed _) = r in error ("time cannot parse: " ++ show tparsed ++ " " ++ fmt)
                    (Right (_, rm), _)        -> error ("remaining string after parse: " ++ rm)
        , testProperty "timezone" $ \tz ->
            let r = timeParseE "TZHM" (show tz) in
            case r of
                Right (localtime, "") -> tz `eq` localTimeGetTimezone localtime
                _                     -> error "Cannot parse timezone"
        ]
    ]
  where toCalendarTest :: (Int, (Elapsed, DateTime)) -> Test
        toCalendarTest (i, (us, dt)) =
            testCase (show i) (dt @=? timeGetDateTimeOfDay us)
        toSecondTest :: (Int, (Elapsed, DateTime)) -> Test
        toSecondTest (i, (us@(Elapsed (Seconds s)), dt)) =
            testCase (show i ++ "-" ++ show s ++ "s") (us @=? timeGetElapsed dt)
        toWeekDayTest :: (Int, (Elapsed, WeekDay)) -> Test
        toWeekDayTest (i, (us, wd)) =
            testCase (show i ++ "-" ++ show wd) (wd @=? getWeekDay (dtDate $ timeGetDateTimeOfDay us))

        tuple12 (a,b,_,_) = (a,b)
        tuple13 (a,_,b,_) = (a,b)

        calTimeFormatTimeISO8601 timePosix =
            T.formatTime T.defaultTimeLocale "%F" (T.posixSecondsToUTCTime timePosix)

main = do
    knowns <- E.catch (map parseTimeConv . lines <$> readFile "test-time-db")
                      (\(_ :: E.SomeException) -> return [])
    defaultMain (tests knowns)
