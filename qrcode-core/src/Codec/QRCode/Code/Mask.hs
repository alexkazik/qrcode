{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codec.QRCode.Code.Mask
  ( applyMask
  , getPenaltyScore
  ) where

import           Codec.QRCode.Base

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

import           Codec.QRCode.Code.Image
import           Codec.QRCode.Data.Mask
import           Codec.QRCode.Data.MQRImage
import           Codec.QRCode.Data.QRImage

-- | Apply the mask to the image, modules marked for functions are excluded.
applyMask :: forall m. PrimMonad m => MQRImage3 (PrimState m) -> Mask -> m ()
applyMask img@MQRImage3{..} m = do
  -- draw format information
  drawFormatBits img m
  -- select correct mask
  case m of
    Mask0 -> go (\x y -> (x + y) `mod` 2 == 0)
    Mask1 -> go (\_ y -> y `mod` 2 == 0)
    Mask2 -> go (\x _ -> x `mod` 3 == 0)
    Mask3 -> go (\x y -> (x + y) `mod` 3 == 0)
    Mask4 -> go (\x y -> (x `div` 3 + y `div` 2) `mod` 2 == 0)
    Mask5 -> go (\x y -> x * y `mod` 2 + x * y `mod` 3 == 0)
    Mask6 -> go (\x y -> (x * y `mod` 2 + x * y `mod` 3) `mod` 2 == 0)
    Mask7 -> go (\x y -> ((x + y) `mod` 2 + x * y `mod` 3) `mod` 2 == 0)
  where
    go :: (Int -> Int -> Bool) -> m ()
    go m' =
      -- iterate over all modules
      forM_ [0 .. (mqrImage3Size * mqrImage3Size) - 1] $ \pos -> do
        let
          (y, x) = pos `divMod` mqrImage3Size
        -- when it's not a function module and the mask tells to invert, do it
        when (not (mqrImage3Fixed UV.! pos) && m' x y) $
          MUV.modify mqrImage3Data not pos

-- | Calculate the penalty score for an image
getPenaltyScore :: QRImage -> Int
getPenaltyScore QRImage{..} = runST $ do
  result <- newSTRef 0
  forM_ [0 .. qrImageSize-1] $ \y -> do
    -- Adjacent modules in row having same color
    ffoldlM_ (False, 0 :: Int) [0 .. qrImageSize-1] $ \(pp, run) x ->
      case p x y of
        np
          | pp /= np ->
              return (np, 1)
          | run < 5 ->
              return (pp, run+1)
          | run == 5 -> do
              modifySTRef' result (+penaltyN1)
              return (pp, run+1)
          | otherwise -> do
              modifySTRef' result (+1)
              return (pp, run+1)
    -- Adjacent modules in column having same color
    ffoldlM_ (False, 0 :: Int) [0 .. qrImageSize-1] $ \(pp, run) x ->
      case p y x of
        np
          | pp /= np ->
              return (np, 1)
          | run < 5 ->
              return (pp, run+1)
          | run == 5 -> do
              modifySTRef' result (+penaltyN1)
              return (pp, run+1)
          | otherwise -> do
              modifySTRef' result (+1)
              return (pp, run+1)

  -- 2*2 blocks of modules having same color
  forM_ [0 .. qrImageSize-2] $ \y ->
    forM_ [0 .. qrImageSize-2] $ \x -> do
      let
        pxy = p x y
      when (pxy == p (x+1) y && pxy == p x (y+1) && pxy == p (x+1) (y+1)) $
        modifySTRef' result (+penaltyN2)

  forM_ [0 .. qrImageSize-1] $ \y -> do
    -- Finder-like pattern in rows
    ffoldlM_ (0 :: Int) [0 .. qrImageSize-1] $ \bits' x -> do
      let
        bits = ((bits' `shiftL` 1) .&. 0x7ff) .|. bool 0 1 (p x y)
      when (x >= 10 && (bits == 0b00001011101 || bits == 0b10111010000)) $
        modifySTRef' result (+penaltyN3)
      return bits
    -- Finder-like pattern in columns
    ffoldlM_ (0 :: Int) [0 .. qrImageSize-1] $ \bits' x -> do
      let
        bits = ((bits' `shiftL` 1) .&. 0x7ff) .|. bool 0 1 (p y x)
      when (x >= 10 && (bits == 0b00001011101 || bits == 0b10111010000)) $
        modifySTRef' result (+penaltyN3)
      return bits

  -- Balance of black and white modules
  let
    black = UV.foldl' (\c pxy -> c + bool 0 1 pxy) 0 qrImageData
    halfOfTotalMulTwo = qrImageSize * qrImageSize
    differenceToMiddleMulTwo = abs (black*2 - halfOfTotalMulTwo)
    steps = (differenceToMiddleMulTwo * 10) `div` halfOfTotalMulTwo
  modifySTRef' result (+ (steps * penaltyN4))

  readSTRef result
  where
    {-# INLINE ffoldlM_ #-}
    ffoldlM_ a b c = void $ foldlM c a b
    {-# INLINE p #-}
    p x y = qrImageData UV.! (x + y * qrImageSize)
    penaltyN1 = 3
    penaltyN2 = 3
    penaltyN3 = 40
    penaltyN4 = 10
