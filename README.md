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

