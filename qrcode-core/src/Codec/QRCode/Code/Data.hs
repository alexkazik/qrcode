{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.QRCode.Code.Data
  ( qrSize
  , QRInternal
  , calcVersionAndErrorLevel
  , appendEndAndPadding
  , appendErrorCorrection
  ) where

import           Codec.QRCode.Base

import qualified Data.Vector.Unboxed                       as UV

import           Codec.QRCode.Code.ReedSolomonEncoder
import qualified Codec.QRCode.Data.ByteStreamBuilder       as BSB
import           Codec.QRCode.Data.ErrorLevel
import           Codec.QRCode.Data.Mask
import           Codec.QRCode.Data.QRCodeOptions
import           Codec.QRCode.Data.QRIntermediate.Internal
import           Codec.QRCode.Data.QRSegment.Internal
import           Codec.QRCode.Data.Result
import           Codec.QRCode.Data.Version

-- | Calculates the size of a QR code (in modules) based on the version.
qrSize :: Version -> Int
{-# INLINE qrSize #-}
qrSize ver = 17 + unVersion ver * 4

-- | The data all encoder pass around
type QRInternal t = (Version, ErrorLevel, t, Maybe Mask)

-- | Determine `Version` and `ErrorLevel` based on the `QRCodeOptions` and the data to encode.
calcVersionAndErrorLevel :: QRCodeOptions -> QRSegment -> Result QRIntermediate
calcVersionAndErrorLevel QRCodeOptions{..} input =
  -- Run though all tree `VersionRange`s and return the first matching.
  -- This ensures that the input stream is only encoded once per `VersionRange` and not for each `Version`.
  firstSuccess checkSizeVR [minBound .. maxBound]
  where
    -- Run though all `Version`s of the `VersionRange` which are permitted by the options and return the first matching.
    checkSizeVR :: VersionRange -> Result QRIntermediate
    checkSizeVR vr = do
      let
        versions = versionsInRangeLimitedBy vr qroMinVersion qroMaxVersion
      guard (not (null versions))
      stream <- unQRSegment input vr
      firstSuccess (checkSize stream) versions
    -- Check if the data fits into a specific `Version`.
    checkSize :: BSB.ByteStreamBuilder -> Version -> Result QRIntermediate
    checkSize bs v = do
      let
        bsl = BSB.length bs
      -- Try all allowed `ErrorLevel`s and chose the one with most error correction which fits the data.
      el <- firstMatch (\e -> bsl <= 8 * numDataCodeWords v e) errorLevels
      pure $
        QRIntermediate v el bsl bs qroMask
    -- Allowed `ErrorLevel`s: Either just one, or the specified and all with "better" error correction if boost is selected.
    errorLevels :: [ErrorLevel]
    errorLevels
      | qroBoostErrorLevel = [H, Q .. qroErrorLevel]
      | otherwise = [qroErrorLevel]
    -- Helper to pick the first successful calculation.
    firstSuccess :: (a -> Result b) -> [a] -> Result b
    firstSuccess fn = foldr ((<|>) . fn) empty
    -- Helper to pick the first matching result.
    firstMatch :: (a -> Bool) -> [a] -> Result a
    firstMatch fn = firstSuccess (\e -> bool empty (pure e) (fn e))

-- | Add the End marker, pad to a full byte (with 0) and pad all further unused bytes (with 0xEC11).
appendEndAndPadding :: QRIntermediate -> QRInternal BSB.ByteStreamBuilder
appendEndAndPadding (QRIntermediate v e bsl bs mmask) =
  let
    -- Capacity of the data part
    capacity = 8 * numDataCodeWords v e
    -- The number of End bits to add (may be less than 4 if there is not enough space)
    endLen = 4 `min` (capacity - bsl)
    -- Pad until a full Byte
    pad0Len = negate (bsl + endLen) `mod` 8
    -- Pad all other unused Bytes
    padEC11Len = capacity - (bsl + endLen + pad0Len)
  in
    (v, e, bs <> BSB.encodeBits (endLen + pad0Len) 0 <> BSB.fromList (take (padEC11Len `div` 8) (cycle [0xec, 0x11])), mmask)

-- | Append the appropriate error correction to the data.
appendErrorCorrection :: QRInternal BSB.ByteStreamBuilder -> QRInternal [Word8]
appendErrorCorrection (v, e, bs, mmask) =
  let
    numBlocks = numErrorCorrectionBlocks v e
    blockEccLen = eccCodeWordsPerBlock v e
    rawCodeWords = numRawDataModules v `div` 8
    numShortBlocks = numBlocks - (rawCodeWords `mod` numBlocks)
    shortBlockLen = rawCodeWords `div` numBlocks
    generatorPolynomial = rsGeneratorPolynomial blockEccLen
    dataBlockLens = [shortBlockLen - blockEccLen + bool 0 1 (x >= numShortBlocks) | x <- [0 .. numBlocks - 1]]
    dataBlocks = snd $ mapAccumL (\da len -> swap (splitAt len da)) (BSB.toList bs) dataBlockLens
    eccBlocks = map (rsEncode generatorPolynomial) dataBlocks
    interleaved = transpose dataBlocks ++ transpose eccBlocks
  in
    (v, e, concat interleaved, mmask)

-- Returns the number of 8-bit data (i.e. not error correction) code words contained in any
-- QR Code of the given version number and error correction level, with remainder bits discarded.
numDataCodeWords :: Version -> ErrorLevel -> Int
{-# INLINABLE numDataCodeWords #-}
numDataCodeWords v e =
  numRawDataModules v `div` 8
  - eccCodeWordsPerBlock v e
  * numErrorCorrectionBlocks v e

-- Returns the number of bits that can be stored in a QR Code of the given version number, after
-- all function modules are excluded. This includes remainder bits, so it might not be a multiple of 8.
numRawDataModules :: Version -> Int
numRawDataModules ver =
  let
    size = qrSize ver
    v2
      | unVersion ver < 2 = 0
      | otherwise =
        let
          numAlign = unVersion ver `div` 7 + 2
        in
           - (numAlign - 1) * (numAlign - 1) * 25 -- Subtract alignment patterns not overlapping with timing patterns
           - (numAlign - 2) * 2 * 20 -- Subtract alignment patterns that overlap with timing patterns
    v7
      | unVersion ver < 7 = 0
      | otherwise = - 18 * 2 -- Subtract version information
  in
    size * size -- Number of modules in the whole QR symbol square
    - 64 * 3 --  Subtract the three finders with separators
    - (15 * 2 + 1) --  Subtract the format information and black module
    - (size - 16) * 2 -- Subtract the timing patterns
    + v2
    + v7

eccCodeWordsPerBlock :: Version -> ErrorLevel -> Int
{-# INLINE eccCodeWordsPerBlock #-}
eccCodeWordsPerBlock v e = eccCodeWordsPerBlockData UV.! (fromEnum e * 40 + unVersion v - 1)

eccCodeWordsPerBlockData :: UV.Vector Int
{-# NOINLINE eccCodeWordsPerBlockData #-}
eccCodeWordsPerBlockData =
  [ --1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40      Error correction level
      7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30,  -- Low
     10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,  -- Medium
     13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30,  -- Quartile
     17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30   -- High
  ]

numErrorCorrectionBlocks :: Version -> ErrorLevel -> Int
{-# INLINE numErrorCorrectionBlocks #-}
numErrorCorrectionBlocks v e = numErrorCorrectionBlocksData UV.! (fromEnum e * 40 + unVersion v - 1)

numErrorCorrectionBlocksData :: UV.Vector Int
{-# NOINLINE numErrorCorrectionBlocksData #-}
numErrorCorrectionBlocksData =
  [ --1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40      Error correction level
      1, 1, 1, 1, 1, 2, 2, 2, 2,  4,  4,  4,  4,  4,  6,  6,  6,  6,  7,  8,  8,  9,  9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25,  -- Low
      1, 1, 1, 2, 2, 4, 4, 4, 5,  5,  5,  8,  9,  9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49,  -- Medium
      1, 1, 2, 2, 4, 4, 6, 6, 8,  8,  8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68,  -- Quartile
      1, 1, 2, 4, 4, 4, 5, 6, 8,  8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81   -- High
  ]
