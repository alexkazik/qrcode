{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Data.ErrorLevel
  ( ErrorLevel(..)
  ) where

import           Codec.QRCode.Base

-- | The error level of an QRCode
data ErrorLevel
  = L -- ^ Allows error recovery up to 7%
  | M -- ^ Allows error recovery up to 15%
  | Q -- ^ Allows error recovery up to 25%
  | H -- ^ Allows error recovery up to 30%
  deriving (Bounded, Enum, Eq)
