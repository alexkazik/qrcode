{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Mode.ECI
  ( eciEx
  , eci
  ) where

import           Codec.QRCode.Base

import qualified Codec.QRCode.Data.ByteStreamBuilder  as BSB
import           Codec.QRCode.Data.QRSegment.Internal
import           Codec.QRCode.Data.Result

eciEx :: Int -> QRSegment
eciEx = constStream . eciExB

-- | Generate a segment representing an Extended Channel Interpretation
--   (ECI) designator with the specified assignment value.
eci :: Int -> Result QRSegment
eci = fmap constStream . eciB

eciExB :: Int -> BSB.ByteStreamBuilder
eciExB = fromMaybe (error "Invalid ECI Code") . getResult . eciB

eciB :: Int -> Result BSB.ByteStreamBuilder
eciB n
  | n < 0       = empty
  | n < 0x80    = pure $ BSB.encodeBits 4 0b0111 <> BSB.encodeBits 8 n
  | n < 0x4000  = pure $ BSB.encodeBits 4 0b0111 <> BSB.encodeBits 2 0b10 <> BSB.encodeBits (2*8-2) n
  | n < 1000000 = pure $ BSB.encodeBits 4 0b0111 <> BSB.encodeBits 3 0b110 <> BSB.encodeBits (3*8-3) n
  | otherwise   = empty
