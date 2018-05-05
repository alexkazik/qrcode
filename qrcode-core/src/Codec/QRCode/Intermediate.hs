{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Intermediate
  (
  -- * Generators
  -- | The results of the different generators can be appended using `<>`.
    mixed
  , automatic
  , text
  , binary
  , kanji
  , alphanumeric
  , numeric
  , eci
  -- * Intermediate
  , intermediate
  -- * Encoder
  , encode
  -- * Re-Exports
  , module Codec.QRCode.Data.ErrorLevel
  , module Codec.QRCode.Data.Mask
  , module Codec.QRCode.Data.QRImage
  , module Codec.QRCode.Data.QRCodeOptions
  , module Codec.QRCode.Data.QRIntermediate
  , module Codec.QRCode.Data.QRSegment
  , module Codec.QRCode.Data.Result
  , module Codec.QRCode.Data.TextEncoding
  , module Codec.QRCode.Data.ToInput
  ) where

import           Codec.QRCode.Code.Intermediate
import           Codec.QRCode.Data.ErrorLevel
import           Codec.QRCode.Data.Mask
import           Codec.QRCode.Data.QRCodeOptions
import           Codec.QRCode.Data.QRImage
import           Codec.QRCode.Data.QRIntermediate
import           Codec.QRCode.Data.QRSegment
import           Codec.QRCode.Data.Result
import           Codec.QRCode.Data.TextEncoding
import           Codec.QRCode.Data.ToInput
import           Codec.QRCode.Mode.Alphanumeric
import           Codec.QRCode.Mode.Automatic
import           Codec.QRCode.Mode.Byte
import           Codec.QRCode.Mode.ECI
import           Codec.QRCode.Mode.Kanji
import           Codec.QRCode.Mode.Mixed
import           Codec.QRCode.Mode.Numeric

-- | Convert segments into an intermediate state.
--   This is the first point where it can be guaranteed that there will
--   be an result. The Version and ErrorLevel is already determined at
--   this point.
intermediate :: QRCodeOptions -> QRSegment -> Result QRIntermediate
{-# INLINE intermediate #-}
intermediate = toIntermediate

-- | Convert the intermediate state into an image.
encode :: QRIntermediate -> QRImage
{-# INLINE encode #-}
encode = fromIntermediate
