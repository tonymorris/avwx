# 0.1.0.2

* Add version bounds to dependencies.
* Turn on GHC warnings
* The parser uses hackage/parsers library, so a user can choose the parser implementation e.g. attoparsec, parsec.
* A METAR optionally ends in $ or = instead of necessarily.
* Added a field to `Weather` for ending in $ (meaning the station requires maintenance).
* Added classy lenses to the data types.
* Updated the new noaa.gov METAR publication website for fetching.
* Moved parseWeather over to the `Fetcher` module so the parser does not ever import attoparsec.
* Make `Station` a newtype.

# Jun 28 2016

* First release.
