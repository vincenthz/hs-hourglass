-- |
-- Module      : Data.Hourglass.Format
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Time formatting : printing and parsing
--
-- Built-in format strings
--
{-# LANGUAGE FlexibleInstances #-}
module Data.Hourglass.Format
    (
    -- * Parsing and Printing
    -- ** Format strings
      TimeFormatElem(..)
    , TimeFormatFct(..)
    , TimeFormatString(..)
    , TimeFormat(..)
    -- ** Common built-in formats
    , ISO8601_Date(..)
    , ISO8601_DateAndTime(..)
    -- ** Format methods
    , timePrint
    , timeParse
    , timeParseE
    , localTimePrint
    , localTimeParse
    , localTimeParseE
    ) where

import Data.Hourglass.Types
import Data.Hourglass.Time
import Data.Hourglass.Calendar
import Data.Hourglass.Local
import Data.Hourglass.Utils
import Data.Char (isDigit, ord)
import Data.Int

-- | All the various formatter that can be part
-- of a time format string
data TimeFormatElem =
      Format_Year2      -- ^ 2 digit years (70 is 1970, 69 is 2069)
    | Format_Year4      -- ^ 4 digits years
    | Format_Year       -- ^ any digits years
    | Format_Month      -- ^ months (1 to 12)
    | Format_Month2     -- ^ months padded to 2 chars (01 to 12)
    | Format_MonthName_Short -- ^ name of the month short ('Jan', 'Feb' ..)
    | Format_DayYear    -- ^ day of the year (1 to 365, 366 for leap years)
    | Format_Day        -- ^ day of the month (1 to 31)
    | Format_Day2       -- ^ day of the month (01 to 31)
    | Format_Hour       -- ^ hours (0 to 23)
    | Format_Minute     -- ^ minutes (0 to 59)
    | Format_Second     -- ^ seconds (0 to 59, 60 for leap seconds)
    | Format_UnixSecond -- ^ number of seconds since 1 jan 1970. unix epoch.
    | Format_MilliSecond -- ^ Milliseconds (000 to 999)
    | Format_MicroSecond -- ^ MicroSeconds (000000 to 999999)
    | Format_NanoSecond  -- ^ NanoSeconds (000000000 to 999999999)
    | Format_Precision Int -- ^ sub seconds display with a precision of N digits. with N between 1 and 9
    | Format_TimezoneName   -- ^ timezone name (e.g. GMT, PST). not implemented yet
    -- | Format_TimezoneOffset -- ^ timeoffset offset (+02:00)
    | Format_TzHM_Colon -- ^ timeoffset offset with colon (+02:00)
    | Format_TzHM       -- ^ timeoffset offset (+0200)
    | Format_Tz_Offset  -- ^ timeoffset in minutes
    | Format_Spaces     -- ^ one or many space-like chars
    | Format_Text Char  -- ^ a verbatim char
    | Format_Fct TimeFormatFct
    deriving (Show,Eq)

-- | A generic format function composed of a parser and a printer.
data TimeFormatFct = TimeFormatFct
    { timeFormatFctName :: String
    , timeFormatParse   :: DateTime -> String -> Either String (DateTime, String)
    , timeFormatPrint   :: DateTime -> String
    }

instance Show TimeFormatFct where
    show f = timeFormatFctName f
instance Eq TimeFormatFct where
    t1 == t2 = timeFormatFctName t1 == timeFormatFctName t2

-- | A time format string, composed of list of 'TimeFormatElem'
newtype TimeFormatString = TimeFormatString [TimeFormatElem]
    deriving (Show,Eq)

-- | A generic class for anything that can be considered a Time Format string.
class TimeFormat format where
    toFormat :: format -> TimeFormatString

-- | ISO8601 Date format string.
--
-- e.g. 2014-04-05
data ISO8601_Date = ISO8601_Date
    deriving (Show,Eq)

-- | ISO8601 Date and Time format string.
--
-- e.g. 2014-04-05T17:25:04+00:00
--      2014-04-05T17:25:04Z
data ISO8601_DateAndTime = ISO8601_DateAndTime
    deriving (Show,Eq)

instance TimeFormat [TimeFormatElem] where
    toFormat = TimeFormatString

instance TimeFormat TimeFormatString where
    toFormat = id

instance TimeFormat String where
    toFormat = TimeFormatString . toFormatElem
      where toFormatElem []                  = []
            toFormatElem ('Y':'Y':'Y':'Y':r) = Format_Year4  : toFormatElem r
            toFormatElem ('Y':'Y':r)         = Format_Year2  : toFormatElem r
            toFormatElem ('M':'M':r)         = Format_Month2 : toFormatElem r
            toFormatElem ('M':'o':'n':r)     = Format_MonthName_Short : toFormatElem r
            toFormatElem ('M':'I':r)         = Format_Minute : toFormatElem r
            toFormatElem ('M':r)             = Format_Month  : toFormatElem r
            toFormatElem ('D':'D':r)         = Format_Day2   : toFormatElem r
            toFormatElem ('H':r)             = Format_Hour   : toFormatElem r
            toFormatElem ('S':r)             = Format_Second : toFormatElem r
            toFormatElem ('m':'s':r)         = Format_MilliSecond : toFormatElem r
            toFormatElem ('u':'s':r)         = Format_MicroSecond : toFormatElem r
            toFormatElem ('μ':r)             = Format_MicroSecond : toFormatElem r
            toFormatElem ('n':'s':r)         = Format_NanoSecond : toFormatElem r
            toFormatElem ('p':'1':r)         = Format_Precision 1 : toFormatElem r
            toFormatElem ('p':'2':r)         = Format_Precision 2 : toFormatElem r
            toFormatElem ('p':'3':r)         = Format_Precision 3 : toFormatElem r
            toFormatElem ('p':'4':r)         = Format_Precision 4 : toFormatElem r
            toFormatElem ('p':'5':r)         = Format_Precision 5 : toFormatElem r
            toFormatElem ('p':'6':r)         = Format_Precision 6 : toFormatElem r
            toFormatElem ('p':'7':r)         = Format_Precision 7 : toFormatElem r
            toFormatElem ('p':'8':r)         = Format_Precision 8 : toFormatElem r
            toFormatElem ('p':'9':r)         = Format_Precision 9 : toFormatElem r
            -----------------------------------------------------------
            toFormatElem ('E':'P':'O':'C':'H':r) = Format_UnixSecond : toFormatElem r
            -----------------------------------------------------------
            toFormatElem ('T':'Z':'H':'M':r)     = Format_TzHM : toFormatElem r
            toFormatElem ('T':'Z':'H':':':'M':r) = Format_TzHM_Colon : toFormatElem r
            toFormatElem ('T':'Z':'O':'F':'S':r) = Format_Tz_Offset : toFormatElem r
            -----------------------------------------------------------
            toFormatElem ('\\':c:r)          = Format_Text c : toFormatElem r
            toFormatElem (' ':r)             = Format_Spaces : toFormatElem r
            toFormatElem (c:r)               = Format_Text c : toFormatElem r

instance TimeFormat ISO8601_Date where
    toFormat _ = TimeFormatString [Format_Year,dash,Format_Month2,dash,Format_Day2]
      where dash = Format_Text '-'

instance TimeFormat ISO8601_DateAndTime where
    toFormat _ = TimeFormatString
        [Format_Year,dash,Format_Month2,dash,Format_Day2 -- date
        ,Format_Text 'T'
        ,Format_Hour,colon,Format_Minute,colon,Format_Second -- time
        ,Format_TzHM_Colon -- timezone offset with colon +HH:MM
        ]
      where dash = Format_Text '-'
            colon = Format_Text ':'

monthFromShort :: String -> Either String Month
monthFromShort str =
    case str of
        "Jan" -> Right January
        "Feb" -> Right February
        "Mar" -> Right March
        "Apr" -> Right April
        "May" -> Right May
        "Jun" -> Right June
        "Jul" -> Right July
        "Aug" -> Right August
        "Sep" -> Right September
        "Oct" -> Right October
        "Nov" -> Right November
        "Dec" -> Right December
        _     -> Left $ "unknown month: " ++ str

printWith :: (TimeFormat format, Timeable t)
          => format
          -> TimezoneOffset
          -> t
          -> String
printWith fmt tzOfs@(TimezoneOffset tz) t = concatMap fmtToString fmtElems
  where fmtToString Format_Year     = show (dateYear date)
        fmtToString Format_Year4    = pad4 (dateYear date)
        fmtToString Format_Year2    = pad2 (dateYear date-1900)
        fmtToString Format_Month2   = pad2 (fromEnum (dateMonth date)+1)
        fmtToString Format_Month    = show (fromEnum (dateMonth date)+1)
        fmtToString Format_MonthName_Short = take 3 $ show (dateMonth date)
        fmtToString Format_Day2     = pad2 (dateDay date)
        fmtToString Format_Day      = show (dateDay date)
        fmtToString Format_Hour     = pad2 (fromIntegral (todHour tm) :: Int)
        fmtToString Format_Minute   = pad2 (fromIntegral (todMin tm) :: Int)
        fmtToString Format_Second   = pad2 (fromIntegral (todSec tm) :: Int)
        fmtToString Format_MilliSecond = padN 3 (ns `div` 1000000)
        fmtToString Format_MicroSecond = padN 3 ((ns `div` 1000) `mod` 1000)
        fmtToString Format_NanoSecond = padN 3 (ns `mod` 1000)
        fmtToString (Format_Precision n)
            | n >= 1 && n <= 9 = padN n (ns `div` (10 ^ (9 - n)))
            | otherwise        = error "invalid precision format"
        fmtToString Format_UnixSecond = show unixSecs
        fmtToString Format_TimezoneName   = "" --
        fmtToString Format_Tz_Offset = show tz
        fmtToString Format_TzHM = show tzOfs
        fmtToString Format_TzHM_Colon =
            let (tzH, tzM) = abs tz `divMod` 60
                sign = if tz < 0 then "-" else "+"
             in sign ++ pad2 tzH ++ ":" ++ pad2 tzM
        fmtToString Format_Spaces   = " "
        fmtToString (Format_Text c) = [c]
        fmtToString f = error ("implemented printing format: " ++ show f)

        (TimeFormatString fmtElems) = toFormat fmt

        (Elapsed (Seconds unixSecs)) = timeGetElapsed t
        (DateTime date tm) = timeGetDateTimeOfDay t
        (NanoSeconds ns) = timeGetNanoSeconds t

-- | Pretty print local time to a string.
--
-- The actual output is determined by the format used.
localTimePrint :: (TimeFormat format, Timeable t)
               => format      -- ^ the format to use for printing
               -> LocalTime t -- ^ the local time to print
               -> String      -- ^ the resulting local time string
localTimePrint fmt lt = localTimeUnwrap $ fmap (printWith fmt (localTimeGetTimezone lt)) lt

-- | Pretty print time to a string
--
-- The actual output is determined by the format used
timePrint :: (TimeFormat format, Timeable t)
          => format -- ^ the format to use for printing
          -> t      -- ^ the global time to print
          -> String -- ^ the resulting string
timePrint fmt t = printWith fmt timezone_UTC t

-- | Try parsing a string as time using the format explicitely specified
--
-- On failure, the parsing function returns the reason of the failure.
-- If parsing is successful, return the date parsed with the remaining unparsed string
localTimeParseE :: TimeFormat format
                => format -- ^ the format to use for parsing
                -> String -- ^ the string to parse
                -> Either (TimeFormatElem, String) (LocalTime DateTime, String)
localTimeParseE fmt timeString = loop ini fmtElems timeString
  where (TimeFormatString fmtElems) = toFormat fmt

        toLocal (dt, tz) = localTime tz dt

        loop acc []    s  = Right (toLocal acc, s)
        loop _   (x:_) [] = Left (x, "empty")
        loop acc (x:xs) s =
            case processOne acc x s of
                Left err         -> Left (x, err)
                Right (nacc, s') -> loop nacc xs s'

        processOne _   _               []     = Left "empty"
        processOne acc (Format_Text c) (x:xs)
            | c == x    = Right (acc, xs)
            | otherwise = Left ("unexpected char, got: " ++ show c)

        processOne acc Format_Year s =
            onSuccess (\y -> modDate (setYear y) acc) $ isNumber s
        processOne acc Format_Year4 s =
            onSuccess (\y -> modDate (setYear y) acc) $ getNDigitNum 4 s
        processOne acc Format_Year2 s = onSuccess
            (\y -> let year = if y < 70 then y + 2000 else y + 1900 in modDate (setYear year) acc)
            $ getNDigitNum 2 s
        processOne acc Format_Month2 s =
            onSuccess (\m -> modDate (setMonth $ toEnum ((fromIntegral m - 1) `mod` 12)) acc) $ getNDigitNum 2 s
        processOne acc Format_MonthName_Short s =
            onSuccess (\m -> modDate (setMonth m) acc) $ getMonth s
        processOne acc Format_Day2 s =
            onSuccess (\d -> modDate (setDay d) acc) $ getNDigitNum 2 s
        processOne acc Format_Hour s =
            onSuccess (\h -> modTime (setHour h) acc) $ getNDigitNum 2 s
        processOne acc Format_Minute s =
            onSuccess (\mi -> modTime (setMin mi) acc) $ getNDigitNum 2 s
        processOne acc Format_Second s =
            onSuccess (\sec -> modTime (setSec sec) acc) $ getNDigitNum 2 s
        processOne acc Format_MilliSecond s =
            onSuccess (\ms -> modTime (setNsMask (6,3) ms) acc) $ getNDigitNum 3 s
        processOne acc Format_MicroSecond s =
            onSuccess (\us -> modTime (setNsMask (3,3) us) acc) $ getNDigitNum 3 s
        processOne acc Format_NanoSecond s =
            onSuccess (\ns -> modTime (setNsMask (0,3) ns) acc) $ getNDigitNum 3 s
        processOne acc (Format_Precision p) s =
            onSuccess (\num -> modTime (setNS num) acc) $ getNDigitNum p s
        processOne acc Format_UnixSecond s =
            onSuccess (\sec ->
                let newDate = dateTimeFromUnixEpochP $ flip ElapsedP 0 $ Elapsed $ Seconds sec
                 in modDT (const newDate) acc) $ isNumber s
        processOne acc Format_TzHM_Colon (c:s) =
            parseHMSign True acc c s
        processOne acc Format_TzHM (c:s) =
            parseHMSign False acc c s

        processOne acc Format_Spaces (' ':s) = Right (acc, s)
        -- catch all for unimplemented format.
        processOne _ f _ = error ("unimplemened parsing format: " ++ show f)

        parseHMSign expectColon acc signChar afterSign =
            case signChar of
                '+' -> parseHM False expectColon afterSign acc
                '-' -> parseHM True expectColon afterSign acc
                _   -> parseHM False expectColon (signChar:afterSign) acc

        parseHM isNeg True (h1:h2:':':m1:m2:xs) acc
            | allDigits [h1,h2,m1,m2] = let tz = toTZ isNeg h1 h2 m1 m2
                                         in Right (modTZ (const tz) acc, xs)
            | otherwise               = Left ("not digits chars: " ++ show [h1,h2,m1,m2])
        parseHM isNeg False (h1:h2:m1:m2:xs) acc
            | allDigits [h1,h2,m1,m2] = let tz = toTZ isNeg h1 h2 m1 m2
                                         in Right (modTZ (const tz) acc, xs)
            | otherwise               = Left ("not digits chars: " ++ show [h1,h2,m1,m2])
        parseHM _ _    _ _ = Left ("invalid timezone format")

        toTZ isNeg h1 h2 m1 m2 = TimezoneOffset ((if isNeg then negate else id) minutes)
          where minutes = (toInt [h1,h2] * 60) + toInt [m1,m2]

        onSuccess f (Right (v, s')) = Right (f v, s')
        onSuccess _ (Left s)        = Left s

        isNumber :: Num a => String -> Either String (a, String)
        isNumber s =
            case span isDigit s of
                ("",s2) -> Left ("no digits chars:" ++ s2)
                (s1,s2) -> Right (toInt s1, s2)

        getNDigitNum :: Int -> String -> Either String (Int64, String)
        getNDigitNum n s =
            case getNChar n s of
                Left err                            -> Left err
                Right (s1, s2) | not (allDigits s1) -> Left ("not a digit chars in " ++ show s1)
                               | otherwise          -> Right (toInt s1, s2)

        getMonth :: String -> Either String (Month, String)
        getMonth s =
            getNChar 3 s >>= \(s1, s2) -> monthFromShort s1 >>= \m -> Right (m, s2)

        getNChar :: Int -> String -> Either String (String, String)
        getNChar n s
            | length s1 < n = Left ("not enough chars: expecting " ++ show n ++ " got " ++ show s1)
            | otherwise     = Right (s1, s2)
          where
                (s1, s2) = splitAt n s

        toInt :: Num a => String -> a
        toInt = foldl (\acc w -> acc * 10 + fromIntegral (ord w - ord '0')) 0

        allDigits = and . map isDigit

        ini = (DateTime (Date 0 (toEnum 0) 0) (TimeOfDay 0 0 0 0), TimezoneOffset 0)

        modDT   f (dt, tz) = (f dt, tz)
        modDate f (DateTime d tp, tz) = (DateTime (f d) tp, tz)
        modTime f (DateTime d tp, tz) = (DateTime d (f tp), tz)
        modTZ   f (dt, tz) = (dt, f tz)

        setYear :: Int64 -> Date -> Date
        setYear  y (Date _ m d) = Date (fromIntegral y) m d
        setMonth m (Date y _ d) = Date y m d
        setDay   d (Date y m _) = Date y m (fromIntegral d)
        setHour  h (TimeOfDay _ m s ns) = TimeOfDay (Hours h) m s ns
        setMin   m (TimeOfDay h _ s ns) = TimeOfDay h (Minutes m) s ns
        setSec   s (TimeOfDay h m _ ns) = TimeOfDay h m (Seconds s) ns
        setNS    v (TimeOfDay h m s _ ) = TimeOfDay h m s (NanoSeconds v)

        setNsMask :: (Int, Int) -> Int64 -> TimeOfDay -> TimeOfDay
        setNsMask (shift, mask) val (TimeOfDay h mins seconds (NanoSeconds ns)) =
            let (nsD,keepL) = ns `divMod` s
                (keepH,_)   = nsD `divMod` m
                v           = ((keepH * m + fromIntegral val) * s) + keepL
             in TimeOfDay h mins seconds (NanoSeconds v)
          where s = 10 ^ shift
                m = 10 ^ mask
-- | Try parsing a string as time using the format explicitely specified
--
-- Unparsed characters are ignored and the error handling is simplified
--
-- for more elaborate need use 'localTimeParseE'.
localTimeParse :: TimeFormat format
               => format -- ^ the format to use for parsing
               -> String -- ^ the string to parse
               -> Maybe (LocalTime DateTime)
localTimeParse fmt s = either (const Nothing) (Just . fst) $ localTimeParseE fmt s

-- | like 'localTimeParseE' but the time value is automatically converted to global time.
timeParseE :: TimeFormat format => format -> String
           -> Either (TimeFormatElem, String) (DateTime, String)
timeParseE fmt timeString = either Left (\(d,s) -> Right (localTimeToGlobal d, s))
                          $ localTimeParseE fmt timeString

-- | Just like 'localTimeParse' but the time is automatically converted to global time.
timeParse :: TimeFormat format => format -> String -> Maybe DateTime
timeParse fmt s = localTimeToGlobal `fmap` localTimeParse fmt s
