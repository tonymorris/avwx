[![Build Status](https://travis-ci.org/hce/avwx.svg?branch=master)](https://travis-ci.org/hce/avwx)

avwx
====

Fetch and parse aviation weather reports.

Currently, only standard METAR reports are supported, subject to extensive
testing. RMK sections are not yet supported. Making this parser work with all METARs is pretty tedious and difficult,
because every country seems to be using some subtly different specification.
This parser was written with the publicly available documentation from
DWD (Deutscher Wetterdienst) in mind, so it should work with all German
METARs. Patches (pull requests) and suggestions are very welcome!

Important: "For educational purposes only, *not* for flight planning! Use at
your own risk."

# Usage

This package is intended to be used as a library. However, for demonstration purposes,
it comes with a small command line utility that can be invoked like this:

    hc@hc-pc ~ λ metar eddf llbg eham tncm

The output will look like this:

    Fetching weather information for eddf
    Parsing "METAR EDDF 281650Z 15006KT 9999 FEW034 SCT048 BKN130 22/15 Q1015 NOSIG="
    METAR
      { date = Date 28 16 50
      , station = ICAO "EDDF"
      , flags = []
      , wind =
          Just
            Wind
              { direction = Degrees 150 , velocity = Knots 6 , gusts = Nothing }
      , visibility = [ TenOrMore ]
      , runwayvis = []
      , runwaycond = []
      , wx = []
      , clouds =
          [ Cloud FEW (Height 3400) Unclassified
          , Cloud SCT (Height 4800) Unclassified
          , Cloud BKN (Height 13000) Unclassified
          ]
      , pressure = Just (QNH 1015)
      , temperature = Just 22
      , dewPoint = Just 15
      , trend = NOSIG
      , remark = Nothing
      , maintenance = False
          }
    ...
