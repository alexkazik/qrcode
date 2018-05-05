{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Data.QRSegment.Internal
  ( QRSegment(..)
  , constStream
  , encodeBits
  , lengthSegment
  ) where

import           Codec.QRCode.Base

import qualified Codec.QRCode.Data.ByteStreamBuilder as BSB
import           Codec.QRCode.Data.Result
import           Codec.QRCode.Data.Version

-- | An segment of encoded data
newtype QRSegment
  = QRSegment
    { unQRSegment :: VersionRange -> Result BSB.ByteStreamBuilder
    }

instance Semigroup QRSegment where
  {-# INLINE (<>) #-}
  QRSegment a <> QRSegment b = QRSegment $ \v -> (<>) <$> a v <*> b v

constStream :: BSB.ByteStreamBuilder -> QRSegment
{-# INLINABLE constStream #-}
constStream = QRSegment . const . pure

encodeBits :: Int -> Int -> QRSegment
{-# INLINABLE encodeBits #-}
encodeBits len = constStream . BSB.encodeBits len

lengthSegment :: (Int, Int, Int) -> Int -> QRSegment
{-# INLINABLE lengthSegment #-}
lengthSegment (n1_9, n10_26, n27_40) l = QRSegment $ \vr ->
  let
    n =
      case vr of
        Version1to9   -> n1_9
        Version10to26 -> n10_26
        Version27to40 -> n27_40
  in
    if l >= (1 `shiftL` n)
      then empty
      else pure $ BSB.encodeBits n l
