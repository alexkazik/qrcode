# qrcode

QR code library in pure Haskell

## qrcode-core

Basic functionality to create a QRCode.

All modes are supported:
- Numeric (digits only)
- Alphanumeric (digits, letters and some other chars)
- Binary / Text (ISO 8859-1 and UTF-8)
- Kanji

There are function to create those specifically and an auto-detect.

The module `Codec.QRCode` has functions which creates an image out of the input.

The module `Codec.QRCode.Intermediate` has functions to create segments of a
QRCode, join them together and finally create the image.

The core of the resulting image is an `Vector` of `Bool`s, each element
describing a module ("pixel") where `False` is white and `True` is black.

## qrcode-juicypixels

This package adds a function to convert the QRCode into a JuicyPixels image.
And some to create a data url containing the QRCode as a PNG.
