hourglass
=========

[![Build Status](https://travis-ci.org/vincenthz/hs-hourglass.png?branch=master)](https://travis-ci.org/vincenthz/hs-hourglass)
[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

Hourglass is a simple time library.

Documentation: [hourglass on hackage](http://hackage.haskell.org/package/hourglass)

Design
------
Key parts of the design are the Timeable and Time typeclasses.
Time representations of the same time values are interchangeable and easy to convert
between each other. This also allows the user to define new time types that
interact with the same functions as the built-in types.

For example:
```haskell
let dateTime0 =
      DateTime { dtDate = Date { dateYear = 1970, dateMonth = January, dateDay = 1 }
               , dtTime = TimeOfDay {todHour = 0, todMin = 0, todSec = 0, todNSec = 0 }}
    elapsed0 = Elapsed 0

> timeGetElapsed elapsed0 == timeGetElapsed dateTime0
True
> timeGetDate elapsed0 == timeGetDate dateTime0
True
> timePrint "YYYY-MM" elapsed0
"1970-01"
> timePrint "YYYY-MM" dateTime0
"1970-01"
```

Hourglass has the same limitations as your system:

* On 32 bit linux, you can't get a date after the year 2038.
* In Windows 7, you can't get the date before the year 1601.

Comparaison with time
---------------------
* Getting posix time:
```haskell
-- With time
import Data.Time.Clock.POSIX

ptime <- getPOSIXTime

-- With hourglass
import System.Hourglass

ptime <- timeCurrent
```

* Getting the current year:
```haskell
-- With time
import Data.Time.Clock
import Data.Time.Calendar

currentYear <- (\(y,_,_) -> y) . toGregorian . utctDay <$> getCurrentTime

-- With hourglass
import System.Hourglass
import Data.Hourglass

currentYear <- dateYear . timeGetDate <$> timeCurrent
```

* Representating "4th May 1970 15:12:24"
```haskell
-- With time
import Data.Time.Clock
import Data.Time.Calendar

let day = fromGregorian 1970 5 4
    diffTime = secondsToDiffTime (15 * 3600 + 12 * 60 + 24)
in UTCTime day diffTime

-- With hourglass
import Data.Hourglass

DateTime (Date 1970 May 4) (TimeOfDay 15 12 24 0)
```
