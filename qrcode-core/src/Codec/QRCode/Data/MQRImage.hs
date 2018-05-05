{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Codec.QRCode.Data.MQRImage
  ( MQRImage1(..)
  , MQRImage2(..)
  , MQRImage3(..)
  , new
  , unsafeConvert
  , clone
  , unsafeFreeze
  ) where

import           Codec.QRCode.Base

import           Control.Monad.Primitive      (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed          as UV
import qualified Data.Vector.Unboxed.Mutable  as MUV

import           Codec.QRCode.Code.Data
import           Codec.QRCode.Data.ErrorLevel
import           Codec.QRCode.Data.QRImage
import           Codec.QRCode.Data.Version

data MQRImage1 s
  = MQRImage1
    { mqrImage1Size       :: Int
    , mqrImage1Data       :: MUV.MVector s Bool
    , mqrImage1Fixed      :: MUV.MVector s Bool
    , mqrImage1Version    :: Version
    , mqrImage1ErrorLevel :: ErrorLevel
    }

data MQRImage2 s
  = MQRImage2
    { mqrImage2Size       :: Int
    , mqrImage2Data       :: MUV.MVector s Bool
    , mqrImage2Fixed      :: UV.Vector Bool
    , mqrImage2Version    :: Version
    , mqrImage2ErrorLevel :: ErrorLevel
    }

data MQRImage3 s
  = MQRImage3
    { mqrImage3Size       :: Int
    , mqrImage3Data       :: MUV.MVector s Bool
    , mqrImage3Fixed      :: UV.Vector Bool
    , mqrImage3Version    :: Version
    , mqrImage3ErrorLevel :: ErrorLevel
    }

new :: PrimMonad m => Version -> ErrorLevel -> m (MQRImage1 (PrimState m))
new v e = do
  let
    size = qrSize v
  img <- MUV.new (size * size)
  MUV.set img False
  fix <- MUV.new (size * size)
  MUV.set fix False
  return
    MQRImage1
      { mqrImage1Size = size
      , mqrImage1Data = img
      , mqrImage1Fixed = fix
      , mqrImage1Version = v
      , mqrImage1ErrorLevel = e
      }

unsafeConvert :: PrimMonad m => MQRImage1 (PrimState m) -> m (MQRImage2 (PrimState m))
unsafeConvert MQRImage1{..} = do
  fix <- UV.unsafeFreeze mqrImage1Fixed
  return
    MQRImage2
      { mqrImage2Size = mqrImage1Size
      , mqrImage2Data = mqrImage1Data
      , mqrImage2Fixed = fix
      , mqrImage2Version = mqrImage1Version
      , mqrImage2ErrorLevel = mqrImage1ErrorLevel
      }

clone :: PrimMonad m => MQRImage2 (PrimState m) -> m (MQRImage3 (PrimState m))
clone MQRImage2{..} = do
  img <- MUV.clone mqrImage2Data
  return
    MQRImage3
      { mqrImage3Size = mqrImage2Size
      , mqrImage3Data = img
      , mqrImage3Fixed = mqrImage2Fixed
      , mqrImage3Version = mqrImage2Version
      , mqrImage3ErrorLevel = mqrImage2ErrorLevel
      }

unsafeFreeze :: PrimMonad m => MQRImage3 (PrimState m) -> m QRImage
unsafeFreeze MQRImage3{..} = do
  img <- UV.unsafeFreeze mqrImage3Data
  return
    QRImage
      { qrVersion = unVersion mqrImage3Version
      , qrErrorLevel = mqrImage3ErrorLevel
      , qrImageSize = mqrImage3Size
      , qrImageData = img
      }
