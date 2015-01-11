{-# LANGUAGE BangPatterns #-}
module TimeDB (parseTimeConv) where

import Data.Char (isDigit)
import Data.Int
import Data.Hourglass

r :: (Read a, Num a) => String -> a
r !s
    | isNumber s = case reads s of
                    [(n,"")] -> fromIntegral (n :: Int64)
                    []       -> error ("cannot parse anything: " ++ show s)
                    _        -> error ("cannot parse anything: " ++ show s)
    | otherwise  = error ("not a num: " ++ s)
  where
        isNumber []       = False
        isNumber ('-':xs) = allNum xs
        isNumber n@(_:_)  = allNum n

        allNum = all isDigit

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'

parseTimeConv :: String -> (Elapsed, DateTime, WeekDay, Int)
parseTimeConv v =
    case wordsWhen (== ':') v of
        [ts, y, m, d, h, n, s, wd, doy] ->
            ( r ts
            , DateTime (Date (r y) (toEnum $ r m - 1) (r d)) (TimeOfDay (r h) (r n) (r s) 0)
            , read wd
            , r doy)
        l -> error ("invalid line: " ++ show l)
