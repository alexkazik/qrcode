{-# LANGUAGE NoImplicitPrelude #-}

-- | Computes the Reed-Solomon error correction code words for a sequence of data code words at a given degree.

module Codec.QRCode.Code.ReedSolomonEncoder
  ( RsGeneratorPolynomial
  , rsGeneratorPolynomial
  , rsEncode
  ) where

import           Codec.QRCode.Base

import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

newtype RsGeneratorPolynomial
  = RsGeneratorPolynomial (UV.Vector Word8)

-- | Creates a Reed-Solomon ECC generator for the specified degree.
rsGeneratorPolynomial :: Int -> RsGeneratorPolynomial
rsGeneratorPolynomial degree = runST $ do
  coefficients <- MUV.new degree
  -- Start with the monomial x^0
  MUV.set coefficients 0
  MUV.write coefficients (degree-1) 1

  -- Compute the product polynomial (x - r^0) * (x - r^1) * (x - r^2) * ... * (x - r^{degree-1}),
  -- drop the highest term, and store the rest of the coefficients in order of descending powers.
  -- Note that r = 0x02, which is a generator element of this field GF(2^8/0x11D).
  void $ iterateNM degree 1 $ \root -> do
    forM_ [0 .. degree-2] $ \j -> do
      next <- MUV.read coefficients (j+1)
      MUV.modify coefficients (\c -> multiply c root `xor` next) j
    -- calc last (does not have a next)
    MUV.modify coefficients (multiply root) (degree-1)
    return (multiply root 0x02)
  RsGeneratorPolynomial <$> UV.unsafeFreeze coefficients

  where
    iterateNM :: Monad m => Int -> a -> (a -> m a) -> m a
    iterateNM n0 i0 f = go n0 i0
      where
        go n i
          | n <= 0 = return i
          | otherwise = go (n-1) =<< f i

-- | Computes and returns the Reed-Solomon error correction code words for the specified sequence of data codewords.
rsEncode :: RsGeneratorPolynomial -> [Word8] -> [Word8]
rsEncode (RsGeneratorPolynomial coefficients) input = runST $ do
  let
    len = UV.length coefficients
  result <- MUV.new len
  MUV.set result 0
  forM_ input $ \b -> do
    r0 <- MUV.read result 0
    let
      factor = b `xor` r0
    forM_ [1 .. len-1] $ \i -> do
      t <- MUV.read result i
      MUV.write result (i-1) t
    MUV.write result (len-1) 0
    forM_ [0 .. len-1] $ \i ->
      MUV.modify result (\rx -> rx `xor` multiply (coefficients UV.! i) factor) i
  result' <- UV.unsafeFreeze result
  return (UV.toList result')

-- | Returns the product of the two given field elements modulo GF(2^8/0x11D).
multiply :: Word8 -> Word8 -> Word8
{-# INLINABLE multiply #-}
multiply x y =
  let
    step z i =
      (z `shiftL` 1) `xor` ((z `shiftR` 7) * 0x1d)
      `xor` (((y `shiftR` i) .&. 1) * x)
  in
    foldl' step 0 [7, 6 .. 0]
