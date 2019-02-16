{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Data.QRIntermediate.Internal
  ( QRIntermediate(..)
  ) where

import           Codec.QRCode.Base

import qualified Codec.QRCode.Data.ByteStreamBuilder as BSB
import           Codec.QRCode.Data.ErrorLevel
import           Codec.QRCode.Data.Mask
import           Codec.QRCode.Data.Version

data QRIntermediate
  = QRIntermediate
    { qrIntermediateVersion_    :: !Version
    , qrIntermediateErrorLevel_ :: !ErrorLevel
    , qrIntermediateDataSize_   :: !Int
    , qrIntermediateData_       :: !BSB.ByteStreamBuilder
    , qrIntermediateMask_       :: !(Maybe Mask)
    }
