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
import Data.Char (isDigit)

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
    {-
    | Format_MilliSecond
    | Format_MicroSecond
    | Format_NanoSecond -}
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

-- | Pretty print local time to a string.
--
-- The actual output is determined by the format used.
localTimePrint :: (TimeFormat format, Timeable t)
               => format           -- ^ the format to use for printing
               -> LocalTime t      -- ^ the local time to print
               -> LocalTime String -- ^ the resulting local time string
localTimePrint fmt lt = fmap (printWith fmt (localTimeGetTimezone lt)) lt

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

        processOne acc Format_Year    s =
            onSuccess (\y -> modDate (setYear y) acc) $ isNumber s
        processOne acc Format_Year4    s =
            onSuccess (\y -> modDate (setYear y) acc) $ is4Digit s
        processOne acc Format_Year2    s = onSuccess
            (\y -> let year = if y < 70 then y + 2000 else y + 1900 in modDate (setYear year) acc)
            $ is2Digit s
        processOne acc Format_Month2   s =
            onSuccess (\m -> modDate (setMonth $ toEnum ((m - 1) `mod` 12)) acc) $ is2Digit s
        processOne acc Format_Day2     s =
            onSuccess (\d -> modDate (setDay d) acc) $ is2Digit s
        processOne acc Format_Hour s =
            onSuccess (\h -> modTime (setHour h) acc) $ is2Digit s
        processOne acc Format_Minute s =
            onSuccess (\mi -> modTime (setMin mi) acc) $ is2Digit s
        processOne acc Format_Second s =
            onSuccess (\sec -> modTime (setSec sec) acc) $ is2Digit s
        processOne acc Format_UnixSecond s =
            onSuccess (\sec ->
                let newDate = dateTimeFromUnixEpochP $ flip ElapsedP 0 $ Elapsed $ Seconds sec
                 in modDT (const newDate) acc) $ isNumber s
        processOne acc Format_TzHM_Colon (c:s) =
            parseHMSign True acc c s
        processOne acc Format_TzHM (c:s) =
            parseHMSign False acc c s

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
          where minutes = (read [h1,h2] * 60) + read [m1,m2]

        onSuccess f (Right (v, s')) = Right (f v, s')
        onSuccess _ (Left s)        = Left s

        is4Digit (a:b:c:d:s)
            | allDigits [a,b,c,d] = Right (read (a:b:c:d:[]), s)
            | otherwise           = Left ("not digits chars: " ++ show [a,b,c,d])
        is4Digit _                = Left ("not enough chars")

        is2Digit (a:b:s)
            | isDigit a && isDigit b = Right (read (a:b:[]), s)
            | otherwise              = Left ("not digits chars: " ++ show [a,b])
        is2Digit _                 = Left ("not enough chars")

        isNumber :: (Read a, Num a) => String -> Either String (a, String)
        isNumber s =
            case span isDigit s of
                ("",s2) -> Left ("no digits chars:" ++ s2)
                (s1,s2) -> Right (read s1, s2)

        allDigits = and . map isDigit

        ini = (DateTime (Date 0 (toEnum 0) 0) (TimeOfDay 0 0 0 0), TimezoneOffset 0)

        modDT   f (dt, tz) = (f dt, tz)
        modDate f (DateTime d tp, tz) = (DateTime (f d) tp, tz)
        modTime f (DateTime d tp, tz) = (DateTime d (f tp), tz)
        modTZ   f (dt, tz) = (dt, f tz)

        setYear  y (Date _ m d) = Date y m d
        setMonth m (Date y _ d) = Date y m d
        setDay   d (Date y m _) = Date y m d
        setHour  h (TimeOfDay _ m s ns) = TimeOfDay h m s ns
        setMin   m (TimeOfDay h _ s ns) = TimeOfDay h m s ns
        setSec   s (TimeOfDay h m _ ns) = TimeOfDay h m s ns

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
