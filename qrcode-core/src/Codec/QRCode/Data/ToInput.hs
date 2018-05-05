{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Data.ToInput
  ( ToText(..)
  , ToNumeric(..)
  , ToBinary(..)
  ) where

import           Codec.QRCode.Base
import           Data.CaseInsensitive (CI, original)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed  as UV


-- | Conversion into a String and the information if the text is case insensitive (relevant for alphanumeric encoding)
class ToText a where
  toString :: a -> [Char]
  isCI :: a -> Bool

instance ToText [Char] where
  toString = id
  isCI _ = False

instance ToText TL.Text where
  toString = TL.unpack
  isCI _ = False

instance ToText T.Text where
  toString = T.unpack
  isCI _ = False

instance ToText a => ToText (CI a) where
  toString = toString . original
  isCI _ = True


-- | Conversion into an array of digits (each has to be 0-9)
class ToNumeric a where
  toNumeric :: a -> [Int]

instance ToNumeric [Int] where
  toNumeric = id

instance ToNumeric [Char] where
  toNumeric = map (subtract 48 . ord)

instance ToNumeric T.Text where
  toNumeric = toNumeric . T.unpack

instance ToNumeric TL.Text where
  toNumeric = toNumeric . TL.unpack


-- | Conversion into binary data
class ToBinary a where
  toBinary :: a -> [Word8]

instance ToBinary [Word8] where
  toBinary = id

instance ToBinary BS.ByteString where
  toBinary = BS.unpack

instance ToBinary BL.ByteString where
  toBinary = BL.unpack

instance ToBinary (V.Vector Word8) where
  toBinary = V.toList

instance ToBinary (UV.Vector Word8) where
  toBinary = UV.toList

instance ToBinary (SV.Vector Word8) where
  toBinary = SV.toList
