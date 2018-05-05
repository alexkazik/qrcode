{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Data.QRIntermediate
  ( QRIntermediate
  , qrIntermediateVersion
  , qrIntermediateErrorLevel
  , qrIntermediateDataSize
  ) where

import           Codec.QRCode.Base

import           Codec.QRCode.Data.ErrorLevel
import           Codec.QRCode.Data.QRIntermediate.Internal
import           Codec.QRCode.Data.Version

-- | The version of the intermediate result
qrIntermediateVersion :: QRIntermediate -> Int
{-# INLINE qrIntermediateVersion #-}
qrIntermediateVersion = unVersion . qrIntermediateVersion_

-- | The ErrorLevel of the intermediate result
qrIntermediateErrorLevel :: QRIntermediate -> ErrorLevel
{-# INLINE qrIntermediateErrorLevel #-}
qrIntermediateErrorLevel = qrIntermediateErrorLevel_

-- | The size of the data in bits inside the intermediate result
qrIntermediateDataSize :: QRIntermediate -> Int
{-# INLINE qrIntermediateDataSize #-}
qrIntermediateDataSize = qrIntermediateDataSize_
