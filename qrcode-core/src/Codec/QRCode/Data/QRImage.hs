{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Codec.QRCode.Data.QRImage
  ( QRImage(..)
  , toList
  , toMatrix
  ) where

import           Codec.QRCode.Base

import qualified Data.Vector.Unboxed          as UV

import           Codec.QRCode.Data.ErrorLevel

data QRImage
  = QRImage
    { qrVersion    :: !Int
    , qrErrorLevel :: !ErrorLevel
    , qrImageSize  :: !Int
    , qrImageData  :: !(UV.Vector Bool)
    }

-- | Convert the QR code image into a list-type containing all
--   elements from top to bottom and left to right.
--   The values for black/white have to be specified.
toList
  :: (IsList l, Item l ~ a)
  => a -- ^ Value for black modules
  -> a -- ^ Value for white modules
  -> QRImage
  -> l
{-# INLINEABLE toList #-}
toList bl wh QRImage{..} =
  fromListN
    (qrImageSize * qrImageSize)
    (map
      (bool wh bl)
      (UV.toList qrImageData)
    )

-- | Convert the QR code image into a list-type of list-type.
--   The values for black/white have to be specified.
toMatrix
  :: (IsList l, Item l ~ k, IsList k, Item k ~ a)
  => a -- ^ Value for black modules
  -> a -- ^ Value for white modules
  -> QRImage
  -> l
{-# INLINEABLE toMatrix #-}
toMatrix bl wh QRImage{..} =
  fromListN
    qrImageSize
      (map
        go
        [0 .. qrImageSize - 1]
      )
  where
    go ofs =
      fromListN
        qrImageSize
        (map
          (bool wh bl)
          (UV.toList $ UV.take qrImageSize $ UV.drop (ofs * qrImageSize) qrImageData)
        )
