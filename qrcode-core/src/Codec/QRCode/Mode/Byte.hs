{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Mode.Byte
  ( binary
  , text
  , encodeUtf8
  ) where

import           Codec.QRCode.Base

import qualified Codec.QRCode.Data.ByteStreamBuilder  as BSB
import           Codec.QRCode.Data.QRSegment.Internal
import           Codec.QRCode.Data.Result
import           Codec.QRCode.Data.TextEncoding
import           Codec.QRCode.Data.ToInput
import           Codec.QRCode.Mode.ECI

-- | Generate a segment representing the specified binary data in byte mode.
binary :: ToBinary a => a -> QRSegment
binary s = encodeBits 4 0b0100 <> lengthSegment (8, 16, 16) (length s') <> constStream (BSB.fromList s')
  where
    s' :: [Word8]
    s' = toBinary s

-- | Generate a segment representing the specified text data encoded as ISO-8859-1 or UTF-8
--   (with or without ECI) in byte mode.
--
--   Please refer to `TextEncoding` on what the difference is.
--
--   In case you want to encode as ISO-8859-1 and already have a [Word8] or similar
--   you can use 'binary' as it creates the same result.
text :: ToText a => TextEncoding -> a -> Result QRSegment
text te s =
  case te of
    Iso8859_1                 -> textIso8859_1 s'
    Utf8WithoutECI            -> textUtf8WithoutECI s'
    Utf8WithECI               -> textUtf8WithECI s'
    Iso8859_1OrUtf8WithoutECI -> textIso8859_1 s' <|> textUtf8WithoutECI s'
    Iso8859_1OrUtf8WithECI    -> textIso8859_1 s' <|> textUtf8WithECI s'
  where
    s' :: [Char]
    s' = toString s

textIso8859_1 :: [Char] -> Result QRSegment
textIso8859_1 s = binary <$> traverse go s
  where
    go :: Char -> Result Word8
    go c =
      let
        c' = ord c
      in
        if c' >= 0 && c' <= 255
          then pure (fromIntegral c')
          else empty

textUtf8WithoutECI :: [Char] -> Result QRSegment
textUtf8WithoutECI s = binary <$> encodeUtf8 s

textUtf8WithECI :: [Char] -> Result QRSegment
textUtf8WithECI s = (<>) <$> eci 26 <*> textUtf8WithoutECI s

encodeUtf8 :: [Char] -> Result [Word8]
encodeUtf8 = (map fromIntegral <$>) . sequence . go
  where
    go [] = []
    go (c:cs) =
      case ord c of
        oc
          | oc < 0 ->
              [empty]
          | oc < 0x80 ->
              pure oc
            : go cs
          | oc < 0x800 ->
              pure (0xc0 + (oc `shiftR` 6))
            : pure (0x80 + oc .&. 0x3f)
            : go cs
          | oc < 0x10000 ->
              pure (0xe0 + (oc `shiftR` 12))
            : pure (0x80 + ((oc `shiftR` 6) .&. 0x3f))
            : pure (0x80 + oc .&. 0x3f)
            : go cs
          | oc < 0x110000 ->
              pure (0xf0 + (oc `shiftR` 18))
            : pure (0x80 + ((oc `shiftR` 12) .&. 0x3f))
            : pure (0x80 + ((oc `shiftR` 6) .&. 0x3f))
            : pure (0x80 + oc .&. 0x3f)
            : go cs
          | otherwise ->
              [empty]
