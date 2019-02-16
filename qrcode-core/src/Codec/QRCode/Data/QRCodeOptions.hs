{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Data.QRCodeOptions
  ( QRCodeOptions(..)
  , defaultQRCodeOptions
  ) where

import           Codec.QRCode.Base

import           Codec.QRCode.Data.ErrorLevel
import           Codec.QRCode.Data.Mask

data QRCodeOptions
  = QRCodeOptions
    { qroMinVersion      :: !Int          -- ^ Minimal version (i.e. size) the qr code may have
    , qroMaxVersion      :: !Int          -- ^ Maximal version (i.e. size) the qr code may have
    , qroErrorLevel      :: !ErrorLevel   -- ^ Selected error correction level
    , qroBoostErrorLevel :: !Bool         -- ^ Increase error correction level within the same version if possible
    , qroMask            :: !(Maybe Mask) -- ^ Specify a mask to be used, only use it if you know what you're doing
    }

-- | The default options are all versions, boost error level and automatic mask, the error level has always to be specified
defaultQRCodeOptions :: ErrorLevel -> QRCodeOptions
defaultQRCodeOptions e =
  QRCodeOptions 1 40 e True Nothing
