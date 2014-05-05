hourglass
=========

Hourglass is a simple time library.

Documentation: [hourglass on hackage](http://hackage.haskell.org/package/hourglass)

Design
------

One of the key design are the Timeable and Time type classes.
Time representation of the same time value are interchangeable and easy to convert
between each other.  This also allow the user to define new time types that
interact with the same functions as the built-in types.

For example:
```haskell
let dateTime0 = DateTime { dtDate = Date { dateYear = 1970, dateMonth = January, dateDay = 1 }
                         , dtTime = TimeOfDay {todHour = 0, todMin = 0, todSec = 0, todNSec = 0 } }
    elapsed0 = Elasped 0

> timeGetElapsed elapsed0 == timeGetElapsed dateTime0
True
> timeGetDate elapsed0 == timeGetDate dateTime0
True
> timePrint "YYYY-MM" elapsed0
"1970-01"
> timePrint "YYYY-MM" dateTime0
"1970-01"
```

Hourglass has the same limitation as your system:

* On 32 bit linux, you can't get a date after the year 2038.
* In Windows 7, you can't get the date before the year 1601.

Comparaison with time
---------------------

* getting posix time:
```haskell
-- with time
import Data.Time.Clock.POSIX
ptime <- getPOSIXTime

-- with hourglass
import System.Hourglass
ptime <- timeCurrent
```

* getting current date year:
```haskell
-- with time
import Data.Time.Clock
import Data.Time.Calendar
currentYear <- (\(y,_,_) -> y) . toGregorian . utcDay <$> getCurrentTime

-- with hourglass
import System.Hourglass
import Data.Time
currentYear <- dateYear . timeGetDate <$> timeCurrent
```

* creating a time representation of "4th May 1970 15:12:24"
```haskell
-- with time
import Data.Time.Clock
import Date.Time.Calendar

let day = fromGregorian 1970 5 4
    diffTime = secondsToDiffTime (15 * 3600 + 12 * 60 + 24)
in UTCTime day diffTime

-- with hourglass
import Date.Time

DateTime (Date 1970 May 4) (TimeOfDay 15 12 24 0)
```
