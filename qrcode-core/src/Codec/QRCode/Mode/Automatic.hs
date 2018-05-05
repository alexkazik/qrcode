{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Mode.Automatic
  ( automatic
  ) where

import           Codec.QRCode.Base

import           Codec.QRCode.Data.QRSegment.Internal
import           Codec.QRCode.Data.Result
import           Codec.QRCode.Data.TextEncoding
import           Codec.QRCode.Data.ToInput
import           Codec.QRCode.Mode.Alphanumeric
import           Codec.QRCode.Mode.Byte
import           Codec.QRCode.Mode.Kanji
import           Codec.QRCode.Mode.Numeric

-- | Encode a whole string using the mode with the shortest result.
--   Will pick either `numeric`, `alphanumeric`, `kanji` or `text` based on the contents.
--
--   Please refer to the specific documentations for details.
automatic :: ToText a => TextEncoding -> a -> Result QRSegment
automatic te s = numeric s' <|> alphanumeric s' <|> kanji s' <|> text te s
  where
    s' :: [Char]
    s' = toString s
