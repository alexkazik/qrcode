# Changelog for qrcode-core

## 0.9.1 -- 2019-05-23

* Support for primitive 0.7

## 0.9.0 -- 2019-02-16

* Changed UTF-8 encoding: encoding a code point outside the unicode range will
  now fail the encoding, previously a replacement character was inserted
* Removed a, internal only, partial function
* Encoding empty data will result in an empty segment
* Encoding an empty segment fails
* Added functions to create an (non empty) segment without data
* Remove StrictData

## 0.8.0 -- 2019-01-09

* Initial release
