{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Mode.Numeric
  ( numeric
  , numericB
  ) where

import           Codec.QRCode.Base

import qualified Codec.QRCode.Data.ByteStreamBuilder  as BSB
import           Codec.QRCode.Data.QRSegment.Internal
import           Codec.QRCode.Data.Result
import           Codec.QRCode.Data.ToInput

-- | Generate a segment representing the specified string of decimal digits encoded in numeric mode.
numeric :: ToNumeric a => a -> Result QRSegment
numeric s =
  ((encodeBits 4 0b0001 <> lengthSegment (10, 12, 14) (length s')) <>) . constStream <$> numericB s'
  where
    s' :: [Int]
    s' = toNumeric s

numericB :: ToNumeric a => a -> Result BSB.ByteStreamBuilder
numericB s
  | all (\c -> c >= 0 && c <= 9) s' = pure (go s')
  | otherwise = empty
  where
    s' :: [Int]
    s' = toNumeric s
    go :: [Int] -> BSB.ByteStreamBuilder
    go (a:b:c:cs) = BSB.encodeBits 10 (a * 100 + b * 10 + c) <> go cs
    go [a,b]      = BSB.encodeBits 7 (a * 10 + b)
    go [a]        = BSB.encodeBits 4 a
    go []         = mempty
