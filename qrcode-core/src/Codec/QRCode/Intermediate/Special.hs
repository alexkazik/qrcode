{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Intermediate.Special
  ( emptyNumericSegment
  , emptyAlphanumericSegment
  , emptyByteSegment
  , emptyKanjiSegment
  ) where

import           Codec.QRCode.Base

import           Codec.QRCode.Data.QRSegment.Internal

emptyNumericSegment :: QRSegment
emptyNumericSegment =
  encodeBits 4 0b0001 <> lengthSegment (10, 12, 14) 0

emptyAlphanumericSegment :: QRSegment
emptyAlphanumericSegment =
  encodeBits 4 0b0010 <> lengthSegment (9, 11, 13) 0

emptyByteSegment :: QRSegment
emptyByteSegment =
  encodeBits 4 0b0100 <> lengthSegment (8, 16, 16) 0

emptyKanjiSegment :: QRSegment
emptyKanjiSegment =
  encodeBits 4 0b1000 <> lengthSegment (8, 10, 12) 0
