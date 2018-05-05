{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Data.Mask
  ( Mask(..)
  ) where

import           Codec.QRCode.Base

-- | The desired mask for the QRCode
data Mask
  = Mask0
  | Mask1
  | Mask2
  | Mask3
  | Mask4
  | Mask5
  | Mask6
  | Mask7
  deriving (Bounded, Enum, Eq)
