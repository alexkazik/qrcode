{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Data.Version
  ( Version
  , unVersion
  , VersionRange(..)
  , versionsInRangeLimitedBy
  ) where

import           Codec.QRCode.Base

newtype Version
  = Version
  { unVersion_ :: Int
  }

unVersion :: Version -> Int
{-# INLINE unVersion #-}
unVersion = unVersion_

data VersionRange
  = Version1to9
  | Version10to26
  | Version27to40
  deriving (Bounded, Enum, Eq)

versionsInRangeLimitedBy :: VersionRange -> Int -> Int -> [Version]
versionsInRangeLimitedBy Version1to9   start end = map Version [start `max`  1 .. end `min`  9]
versionsInRangeLimitedBy Version10to26 start end = map Version [start `max` 10 .. end `min` 26]
versionsInRangeLimitedBy Version27to40 start end = map Version [start `max` 27 .. end `min` 40]
