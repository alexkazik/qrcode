{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode
  (
  -- * Encoders
    encode
  , encodeAutomatic
  , encodeText
  , encodeBinary
  , encodeKanji
  , encodeAlphanumeric
  , encodeNumeric
  -- * Re-Exports
  , module Codec.QRCode.Data.QRCodeOptions
  , module Codec.QRCode.Data.ErrorLevel
  , module Codec.QRCode.Data.Mask
  , module Codec.QRCode.Data.TextEncoding
  , module Codec.QRCode.Data.ToInput
  , module Codec.QRCode.Data.QRImage
  ) where

import           Codec.QRCode.Base

import           Codec.QRCode.Code.Intermediate
import           Codec.QRCode.Data.ErrorLevel
import           Codec.QRCode.Data.Mask
import           Codec.QRCode.Data.QRCodeOptions
import           Codec.QRCode.Data.QRImage
import           Codec.QRCode.Data.Result
import           Codec.QRCode.Data.TextEncoding
import           Codec.QRCode.Data.ToInput
import           Codec.QRCode.Mode.Alphanumeric
import           Codec.QRCode.Mode.Automatic
import           Codec.QRCode.Mode.Byte
import           Codec.QRCode.Mode.Kanji
import           Codec.QRCode.Mode.Mixed
import           Codec.QRCode.Mode.Numeric

-- | Encode a string into an QR code using any mode that seems fit, will encode the input in parts, each as
--   `encodeNumeric`, `encodeAlphanumeric`, `encodeKanji` and `encodeText` based on the contents.
--
--   Please refer to the specific documentations for details.
--
--   Should result in the shortest encoded data.
encode :: ToText a => QRCodeOptions -> TextEncoding -> a -> Maybe QRImage
{-# INLINABLE encode #-}
encode opt te = getResult . (fromIntermediate <$>) . (toIntermediate opt =<<) . mixed te

-- | Encode a whole string into an QR code using the mode with the shortest result.
--   Will pick either `encodeNumeric`, `encodeAlphanumeric`, `encodeKanji` or `encodeText` based on the contents.
--
--   Please refer to the specific documentations for details.
encodeAutomatic :: ToText a => QRCodeOptions -> TextEncoding -> a -> Maybe QRImage
{-# INLINABLE encodeAutomatic #-}
encodeAutomatic opt te = getResult . (fromIntermediate <$>) . (toIntermediate opt =<<) . automatic te

-- | Generate a QR code representing the specified text data encoded as ISO-8859-1 or UTF-8
--   (with or without ECI) in byte mode.
--
--   Please refer to `TextEncoding` on what the difference is.
--
--   In case you want to encode as ISO-8859-1 and already have a [Word8] or similar
--   you can use 'encodeBinary' as it creates the same result.
encodeText :: ToText a => QRCodeOptions -> TextEncoding -> a -> Maybe QRImage
{-# INLINABLE encodeText #-}
encodeText opt te = getResult . (fromIntermediate <$>) . (toIntermediate opt =<<) . text te

-- | Generate a QR code representing the specified binary data in byte mode.
encodeBinary :: ToBinary a => QRCodeOptions -> a -> Maybe QRImage
{-# INLINABLE encodeBinary #-}
encodeBinary opt = getResult . (fromIntermediate <$>) . toIntermediate opt . binary

-- | Generate a QR code representing the specified text data encoded as QR code Kanji.
--
--   Since this encoding does neither contain ASCII nor the half width katakana characters
--   it may be impossible to encode all text in this encoding.
encodeKanji :: ToText a => QRCodeOptions -> a -> Maybe QRImage
{-# INLINABLE encodeKanji #-}
encodeKanji opt = getResult . (fromIntermediate <$>) . (toIntermediate opt =<<) . kanji

-- | Generate a QR code representing the specified text string encoded in alphanumeric mode.
--
--    The alphanumeric encoding contains this characters: "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:".
--
--    When the input is case insensitive the chars are converted to uppercase since this alphabet contains only uppercase characters.
--    This can be archived by applying `Data.CaseInsensitive.mk` to the input.
encodeAlphanumeric :: ToText a => QRCodeOptions -> a -> Maybe QRImage
{-# INLINABLE encodeAlphanumeric #-}
encodeAlphanumeric opt = getResult . (fromIntermediate <$>) . (toIntermediate opt =<<) . alphanumeric

-- | Generate a QR code representing the specified string of decimal digits encoded in numeric mode.
encodeNumeric :: ToNumeric a => QRCodeOptions -> a -> Maybe QRImage
{-# INLINABLE encodeNumeric #-}
encodeNumeric opt = getResult . (fromIntermediate <$>) . (toIntermediate opt =<<) . numeric
