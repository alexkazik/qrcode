{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Codec.QRCode.Data.ByteStreamBuilder
  ( ByteStreamBuilder
  , encodeBits
  , toList
  , Codec.QRCode.Data.ByteStreamBuilder.length
  , fromList
  , toBitStream
  ) where

import           Codec.QRCode.Base

import qualified Data.DList        as DL

-- | List of bits. Stored as a pair of Int, how many bits to store and the data, in a DList.
--   The DList gives a O(1) append.
--   The number of bits in a pair is never more than 22.
newtype ByteStreamBuilder
  = ByteStreamBuilder
    { unBitStreamBuilder :: DL.DList (Int, Int)
    }

instance Semigroup ByteStreamBuilder where
  {-# INLINE (<>) #-}
  ByteStreamBuilder a <> ByteStreamBuilder b = ByteStreamBuilder (a `DL.append` b)

instance Monoid ByteStreamBuilder where
  {-# INLINE mempty #-}
  mempty = ByteStreamBuilder mempty
#if !(MIN_VERSION_base(4,11,0))
  {-# INLINE mappend #-}
  mappend = (<>)
#endif

-- | Store bits from Int in an ByteStreamBuilder
encodeBits :: Int -> Int -> ByteStreamBuilder
encodeBits n b
  | n <= 0 = mempty
  | n > 22 = encodeBits (n-16) (b `shiftR` 16) <> encodeBits 16 b
  | otherwise = ByteStreamBuilder (DL.singleton (n, b .&. (bit n - 1)))

-- | Store bits from an list of Bytes in an ByteStreamBuilder
fromList :: [Word8] -> ByteStreamBuilder
{-# INLINEABLE fromList #-}
fromList = ByteStreamBuilder . DL.fromList . map ((8,) . fromIntegral)

length :: ByteStreamBuilder -> Int
{-# INLINEABLE length #-}
length = sum . map fst . DL.toList . unBitStreamBuilder

-- | Convert ByteStreamBuilder to list of Word8
toList :: ByteStreamBuilder -> [Word8]
toList = go 0 0 . DL.toList . unBitStreamBuilder
  where
    go :: Int -> Int -> [(Int, Int)] -> [Word8]
    go n b xs
      | n >= 8 =
        fromIntegral (b `shiftR` (n-8)) : go (n-8) b xs
    go n _ ((n', b'):xs)
      | n == 0 && n' == 8 =
        fromIntegral b' : go 0 0 xs -- short circut if we have currently 0 bits and the next chunk contains 8 bits
    go n b ((n', b'):xs) =
      go (n+n') ((b `shiftL` n') .|. b') xs -- maximum leftover: 7, maximum new bits: 22, result is < 30 bits (what a Int can store at least)
    go _ _ [] = []

-- | Convert list of Word8 to list of Bool
toBitStream :: [Word8] -> [Bool]
toBitStream (x:xs) =
    (x .&. 128 /= 0)
  : (x .&.  64 /= 0)
  : (x .&.  32 /= 0)
  : (x .&.  16 /= 0)
  : (x .&.   8 /= 0)
  : (x .&.   4 /= 0)
  : (x .&.   2 /= 0)
  : (x .&.   1 /= 0)
  : toBitStream xs
toBitStream [] = []
