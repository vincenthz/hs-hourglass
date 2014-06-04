## Version 0.2.0 (3 Jun 2014)

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

## Version 0.1.2 (5 May 2014)

- fix compilation on OSX
- add some system benchmarks
- comment and markup reformating

## Version 0.1.1 (4 May 2014)

- add all the cabal tests file to the source dist
- https-ize some urls

## Version 0.1.0 (4 May 2014)

- Initial version
