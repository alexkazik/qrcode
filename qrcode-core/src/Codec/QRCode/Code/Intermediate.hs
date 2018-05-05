{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Code.Intermediate
  ( toIntermediate
  , fromIntermediate
  ) where

import           Codec.QRCode.Base

import           Codec.QRCode.Code.Data
import           Codec.QRCode.Code.Image
import           Codec.QRCode.Code.Mask
import qualified Codec.QRCode.Data.ByteStreamBuilder       as BSB
import           Codec.QRCode.Data.Mask
import qualified Codec.QRCode.Data.MQRImage                as MI
import           Codec.QRCode.Data.QRCodeOptions
import           Codec.QRCode.Data.QRImage
import           Codec.QRCode.Data.QRIntermediate.Internal
import           Codec.QRCode.Data.QRSegment.Internal
import           Codec.QRCode.Data.Result

-- | Convert segments into an intermediate state.
--   This is the first point where it can be guaranteed that there will
--   be an result. The Version and ErrorLevel is already determined at
--   this point.
toIntermediate :: QRCodeOptions -> QRSegment -> Result QRIntermediate
{-# INLINE toIntermediate #-}
toIntermediate = calcVersionAndErrorLevel

-- | Convert the intermediate state into an image.
fromIntermediate :: QRIntermediate -> QRImage
{-# INLINE fromIntermediate #-}
fromIntermediate = generateQRImage . appendErrorCorrection . appendEndAndPadding

-- | "Draw" the image
generateQRImage :: QRInternal [Word8] -> QRImage
generateQRImage (v, e, bs, mmask) = runST $ do
  -- create a new image
  img1 <- MI.new v e
  -- draw all function modules
  drawFunctionPatterns img1
  -- convert the image, now the information wether an module is for data or function can't be changed anymore
  img2 <- MI.unsafeConvert img1
  -- draw the image
  drawCodeWords img2 (BSB.toBitStream bs)
  case mmask of
    Just m -> do
      -- a specific mask was given
      -- clone the current image
      img3 <- MI.clone img2
      -- apply the mask
      applyMask img3 m
      -- return the image
      MI.unsafeFreeze img3
    Nothing -> do
      rs <- forM [Mask0 .. Mask7] $ \m -> do
        -- create a new clone of the image
        img3 <- MI.clone img2
        -- apply the mask
        applyMask img3 m
        -- freeze the image (can't be altered anymore)
        qrimg <- MI.unsafeFreeze img3
        -- return the image along with the penalty score
        return (getPenaltyScore qrimg, qrimg)
      -- pick the image with the lowest penalty score
      return $ snd $ head $ sortOn fst rs
