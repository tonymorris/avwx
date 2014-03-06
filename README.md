avutils
=======

Some aviation related utilities.

I'm writing these to learn haskell and about aviation.

For educational purposes only, *NOT* for flight planning!
Use at your own risk.
Comments and contributions are welcome.

What works:
  * WX:     Weather parser. Currently only parses METARs, excluding TRENDs.

Command line usage
==================

    ./fetchwx eddf edds edfe
Fetch and print the weather of Frankfurt, Stuttgart and Egelsbach

    echo "METAR COR EDDM 061650Z 08007KT CAVOK 08/02 Q1027 NOSIG=" | ./parsewx
To parse one or more wx reports
