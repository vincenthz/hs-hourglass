-- |
-- Module      : Example.Time.Compat
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
--
-- This file is an example on how to use the Data.Hourglass.Compat
-- module to transpose a ZonedTime (from time) into a LocalTime of DateTime
-- (from hourglass).
--
module Example.Time.Compat
    ( transpose
    ) where

import Data.Hourglass        as H
import Data.Hourglass.Compat as C
import Data.Time             as T

transpose :: T.ZonedTime
          -> H.LocalTime H.DateTime
transpose oldTime =
    H.localTime
        offsetTime
        (H.DateTime newDate timeofday)
  where
    (T.ZonedTime (T.LocalTime day tod) (T.TimeZone tzmin _ _)) = oldTime

    newDate :: H.Date
    newDate = C.dateFromTAIEpoch $ T.toModifiedJulianDay day

    timeofday :: H.TimeOfDay
    timeofday = C.diffTimeToTimeOfDay $ toRational $ T.timeOfDayToTime tod

    offsetTime = H.TimezoneOffset $ fromIntegral tzmin
