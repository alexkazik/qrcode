{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.QRCode.Code.Image
  ( drawFunctionPatterns
  , drawFormatBits
  , drawCodeWords
  ) where

import           Codec.QRCode.Base

import           Control.Monad.Primitive      (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed          as UV
import qualified Data.Vector.Unboxed.Mutable  as MUV

import           Codec.QRCode.Data.ErrorLevel
import           Codec.QRCode.Data.Mask
import           Codec.QRCode.Data.MQRImage
import           Codec.QRCode.Data.Version

--
-- Draw (almost) all function patterns into an image
--

-- | Draw all function patterns
drawFunctionPatterns :: PrimMonad m => MQRImage1 (PrimState m) -> m ()
drawFunctionPatterns img@MQRImage1{..} = do
  drawTimingPatterns img -- will be overwritten by finder and alignment patterns
  let
    (alignmentPatternPositions, maxAlignmentPosition) = calculateAlignmentPatternPositions mqrImage1Version
  forM_ [(x,y) | x <- alignmentPatternPositions, y <- alignmentPatternPositions] $ \(x,y) ->
    unless (x == 6 && y == 6 || x == maxAlignmentPosition && y == 6 || x == 6 && y == maxAlignmentPosition) $
      drawAlignmentPattern img x y
  drawFinderPattern img 3 3
  drawFinderPattern img (mqrImage1Size-4) 3
  drawFinderPattern img 3 (mqrImage1Size-4)
  when (unVersion mqrImage1Version >= 7) $
    drawVersion img
  reserveFormatBits img -- will be overwritten later with drawFormatBits
  where
    -- | Calculate all alignment pattern positions
    calculateAlignmentPatternPositions :: Version -> ([Int], Int)
    calculateAlignmentPatternPositions ver
      | unVersion ver == 1 = ([], 0)
      | otherwise =
        let
          numAlign = unVersion ver `div` 7 + 2
          step
            | unVersion ver == 32 = 26
            | otherwise = (unVersion ver * 4 + numAlign * 2 + 1) `div` (2 * numAlign - 2) * 2
          pos p = unVersion ver * 4 + 10 - p * step
        in
          (6 : [ pos p | p <- [0 .. numAlign-2]], pos 0)

-- | Draw both timing patterns (alternate black/white modules)
drawTimingPatterns :: PrimMonad m => MQRImage1 (PrimState m) -> m ()
drawTimingPatterns img@MQRImage1{..} =
  forM_ [0 .. mqrImage1Size-1] $ \i -> do
    setFunctionModule img 6 i (i `mod` 2 == 0)
    setFunctionModule img i 6 (i `mod` 2 == 0)

-- | Draws a 5*5 alignment pattern, with the center module at (x, y)
drawAlignmentPattern :: PrimMonad m => MQRImage1 (PrimState m) -> Int -> Int -> m ()
drawAlignmentPattern img x y =
  forM_ [-2 .. 2] $ \i ->
    forM_ [-2 .. 2] $ \j ->
      setFunctionModule img (x+j) (y+i) ((abs i `max` abs j) /= 1)

-- | Draws a 9*9 finder pattern including the border separator, with the center module at (x, y)
drawFinderPattern :: PrimMonad m => MQRImage1 (PrimState m) -> Int -> Int -> m ()
drawFinderPattern img@MQRImage1{..} x y =
  forM_ [-4 .. 4] $ \i ->
    forM_ [-4 .. 4] $ \j -> do
      let
        dist = abs i `max` abs j
        x' = x + j
        y' = y + i
      when (x' >= 0 && x' < mqrImage1Size && y' >= 0 && y' < mqrImage1Size) $
        setFunctionModule img (x+j) (y+i) (dist /= 2 && dist /= 4)

-- | Draw the version information into the image
drawVersion :: PrimMonad m => MQRImage1 (PrimState m) -> m ()
drawVersion img@MQRImage1{..} = do
  let
    v = unVersion mqrImage1Version
  -- Calculate error correction code and pack bits
    rem' = iterateN 12 v (\r -> (r `shiftL` 1) `xor` ((r `shiftR` 11) * 0x1F25))
    da = (v `shiftL` 12) .|. rem'
  -- Draw two copies
  forM_ [0 .. 17] $ \i -> do
    let
      d = testBit da i
      a = mqrImage1Size - 11 + (i `mod` 3)
      b = i `div` 3
    setFunctionModule img a b d
    setFunctionModule img b a d

-- | Mark all modules which will be used by the format bits as a function pattern
--   (but don't actually write anything into it yet).
reserveFormatBits :: PrimMonad m => MQRImage1 (PrimState m) -> m ()
reserveFormatBits img@MQRImage1{..} = do
  let
    fn x y = MUV.write mqrImage1Fixed (y * mqrImage1Size + x) True

  -- Reserve first copy
  forM_ [0 .. 5] $ \i ->
    fn 8 i
  fn 8 7
  fn 8 8
  fn 7 8
  forM_ [9 .. 14] $ \i ->
    fn (14 - i) 8

  -- Reserve second copy
  forM_ [0 .. 7] $ \i ->
    fn (mqrImage1Size - 1 - i) 8
  forM_ [8 .. 14] $ \i ->
    fn 8 (mqrImage1Size - 15 + i)

  -- Draw fixed set module
  setFunctionModule img 8 (mqrImage1Size - 8) True

--
-- Functions to be used later (once the format / data is determined)
--

-- | Draw the actual format bits into the image
drawFormatBits :: PrimMonad m => MQRImage3 (PrimState m) -> Mask -> m ()
drawFormatBits MQRImage3{..} m = do
  let
    daSource = (errorLevelMask mqrImage3ErrorLevel `shiftL` 3) .|. fromEnum m
    rem' = iterateN 10 daSource (\r -> (r `shiftL` 1) `xor` ((r `shiftR` 9) * 0x537))
    da = ((daSource `shiftL` 10) .|. rem') `xor` 0x5412
    fn x y = MUV.write mqrImage3Data (x + y * mqrImage3Size)

  -- Draw first copy
  forM_ [0 .. 5] $ \i ->
    fn 8 i (testBit da i)
  fn 8 7 (testBit da 6)
  fn 8 8 (testBit da 7)
  fn 7 8 (testBit da 8)
  forM_ [9 .. 14] $ \i ->
    fn (14 - i) 8 (testBit da i)

  -- Draw second copy
  forM_ [0 .. 7] $ \i ->
    fn (mqrImage3Size - 1 - i) 8 (testBit da i)
  forM_ [8 .. 14] $ \i ->
    fn 8 (mqrImage3Size - 15 + i) (testBit da i)

-- | Draw the code words (data and error correction) into the image
drawCodeWords :: PrimMonad m => MQRImage2 (PrimState m) -> [Bool] -> m ()
drawCodeWords MQRImage2{..} d = do
  ffoldlM_ d ([mqrImage2Size-1, mqrImage2Size-3 .. 8] ++ [5, 3, 1]) $ \d' right -> do
    let
      upward = ((right + 1) .&. 2) == 0
    ffoldlM d' (bool [0 .. mqrImage2Size-1] [mqrImage2Size-1, mqrImage2Size-2 .. 0] upward) $ \d'' y ->
      ffoldlM d'' [right, right-1] $ \d''' x -> do
        let
          f = mqrImage2Fixed UV.! (x + y * mqrImage2Size)
        case d''' of
          (isBlack:xs)
            | not f -> do
              when isBlack $
                MUV.write mqrImage2Data (x + y * mqrImage2Size) True -- all unused pixels are already white and do not need to be set
              return xs
          xxs -> return xxs
  return ()
  where
    ffoldlM d' i f = foldlM f d' i
    ffoldlM_ d' i f = void $ foldlM f d' i

--
-- Helper
--

-- | Sets the color of a module and marks it as a function module
setFunctionModule :: PrimMonad m => MQRImage1 (PrimState m) -> Int -> Int -> Bool -> m ()
{-# INLINABLE setFunctionModule #-}
setFunctionModule MQRImage1{..} x y isBlack = do
  MUV.write mqrImage1Data (y * mqrImage1Size + x) isBlack
  MUV.write mqrImage1Fixed (y * mqrImage1Size + x) True

-- | Execute an action n times
iterateN :: Int -> a -> (a -> a) -> a
{-# INLINABLE iterateN #-}
iterateN n0 i0 f = go n0 i0
  where
    go n i
      | n <= 0 = i
      | otherwise = go (n-1) (f i)

-- | The mask value of an ErrorLevel
errorLevelMask :: ErrorLevel -> Int
errorLevelMask L = 1
errorLevelMask M = 0
errorLevelMask Q = 3
errorLevelMask H = 2
