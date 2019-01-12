# Changelog for qrcode-core

## upcoming

* Changed UTF-8 encoding: encoding a code point outside the unicode range will
  now fail the encoding, previously a replacement character was inserted
* Removed a, internal only, partial function
* Encoding empty data will result in an empty segment
* Encoding an empty segment fails
* Added functions to create an (non empty) segment without data

## 0.8.0 -- 2019-01-09

* Initial release
