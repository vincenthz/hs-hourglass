## Version 0.2.8 (2015-01-07)

- Fix test with time 1.5

## Version 0.2.7 (2015-01-07)

- Add compatibility module for easy convertion with time and other standards.
- Format parsing improvements

## Version 0.2.6 (2014-10-19)

- fix compilation of benchs.
- add utc time.
- print the error in the test
- remove all the read instances in favor of explicit parsing in time parsing.

## Version 0.2.5 (2014-10-04)

- Fixed Windows build
- Added type signature to fromIntegral

## Version 0.2.4 (2014-09-30)

- Fix ElapsedP Num instance (addition and substraction)
- add travis machinery

## Version 0.2.3 (2014-09-25)

- Fix build on GNU/Hurd.
- Add milliseconds, microseconds and nanoseconds format time

## Version 0.2.2 (2014-06-11)

- wrap system time in local time correctly

## Version 0.2.1 (2014-06-10)

- unwrap local time structure when doing a localTimePrint
- properly show hours, minutes and seconds in format print.
- add some description of new calls.

## Version 0.2.0 (2014-06-03)

- use tasty to replace test-framework
- add some inlining pragma to tentatively deal with rules properly.
- Remove the Time method to get timezone offset, all local time must be handled
  through LocalTime
- Remove the Time instance for Localtime.
- add localTimeParse since timeParse is not suitable anymore for LocalTime (no
  more time instance).
- add Hours and Minutes types.
- add a time interval class to convert between time unit types.
- add some new derived classes (Enum,Real,Integral) for time unit types.
- split TimeDiff into a Period and Duration structure.

## Version 0.1.2 (2014-05-05)

- fix compilation on OSX
- add some system benchmarks
- comment and markup reformating

## Version 0.1.1 (2014-05-04)

- add all the cabal tests file to the source dist
- https-ize some urls

## Version 0.1.0 (2014-05-04)

- Initial version
