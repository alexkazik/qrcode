# Changelog for qrcode-core

## 0.9.11 -- 2026-02-04

* Support for bytestring-0.12
* Support for containers-0.7

## 0.9.10 -- 2024-09-29

* Support for text-2.1

## 0.9.9 -- 2023-10-10

* Support for primitive-0.9

## 0.9.8 -- 2023-04-30

* Support for primitive-0.8

## 0.9.7 -- 2023-04-23

* Support for vector-0.13

## 0.9.6 -- 2023-03-19

* Support for text-2.0

## 0.9.5 -- 2022-03-16

* Support for bytestring-0.11

## 0.9.4 -- 2020-07-19

* Support for dlist-1.0

## 0.9.3 -- 2020-04-27

* Adapt to base-4.13

## 0.9.2 -- 2019-12-19

* Fix `toMatrix`

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
