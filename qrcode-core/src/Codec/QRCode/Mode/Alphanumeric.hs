{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Codec.QRCode.Mode.Alphanumeric
  ( alphanumeric
  , alphanumericB
  , alphanumericMap
  ) where

import           Codec.QRCode.Base

import qualified Data.Map.Strict                      as M

import qualified Codec.QRCode.Data.ByteStreamBuilder  as BSB
import           Codec.QRCode.Data.QRSegment.Internal
import           Codec.QRCode.Data.Result
import           Codec.QRCode.Data.ToInput

-- | Generate a segment representing the specified text string encoded in alphanumeric mode.
--
--    The alphanumeric encoding contains this characters: "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:".
--
--    When the input is case insensitive the chars are converted to uppercase since this alphabet contains only uppercase characters.
--    This can be archived by applying `Data.CaseInsensitive.mk` to the input.
alphanumeric :: ToText a => a -> Result QRSegment
alphanumeric s =
  ((encodeBits 4 0b0010 <> lengthSegment (9, 11, 13) (length s')) <>) . constStream
  <$> alphanumericB (isCI s) s'
  where
    s' :: [Char]
    s' = toString s

alphanumericB :: Bool -> [Char] -> Result BSB.ByteStreamBuilder
alphanumericB ci s = go <$> traverse (Result . (`M.lookup` alphanumericMap ci)) s
  where
    go :: [Int] -> BSB.ByteStreamBuilder
    go (a:b:cs) = BSB.encodeBits 11 (a * 45 + b) <> go cs
    go [a]      = BSB.encodeBits  6 a
    go []       = mempty
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 800
    go _        = error "This is just to get rid of the Warning."
#endif

alphanumericMap :: Bool -> M.Map Char Int
alphanumericMap False =
  [ ('0',  0)
  , ('1',  1)
  , ('2',  2)
  , ('3',  3)
  , ('4',  4)
  , ('5',  5)
  , ('6',  6)
  , ('7',  7)
  , ('8',  8)
  , ('9',  9)
  , ('A', 10)
  , ('B', 11)
  , ('C', 12)
  , ('D', 13)
  , ('E', 14)
  , ('F', 15)
  , ('G', 16)
  , ('H', 17)
  , ('I', 18)
  , ('J', 19)
  , ('K', 20)
  , ('L', 21)
  , ('M', 22)
  , ('N', 23)
  , ('O', 24)
  , ('P', 25)
  , ('Q', 26)
  , ('R', 27)
  , ('S', 28)
  , ('T', 29)
  , ('U', 30)
  , ('V', 31)
  , ('W', 32)
  , ('X', 33)
  , ('Y', 34)
  , ('Z', 35)
  , (' ', 36)
  , ('$', 37)
  , ('%', 38)
  , ('*', 39)
  , ('+', 40)
  , ('-', 41)
  , ('.', 42)
  , ('/', 43)
  , (':', 44)
  ]
alphanumericMap True =
  [ ('0',  0)
  , ('1',  1)
  , ('2',  2)
  , ('3',  3)
  , ('4',  4)
  , ('5',  5)
  , ('6',  6)
  , ('7',  7)
  , ('8',  8)
  , ('9',  9)
  , ('A', 10)
  , ('a', 10)
  , ('B', 11)
  , ('b', 11)
  , ('C', 12)
  , ('c', 12)
  , ('D', 13)
  , ('d', 13)
  , ('E', 14)
  , ('e', 14)
  , ('F', 15)
  , ('f', 15)
  , ('G', 16)
  , ('g', 16)
  , ('H', 17)
  , ('h', 17)
  , ('I', 18)
  , ('i', 18)
  , ('J', 19)
  , ('j', 19)
  , ('K', 20)
  , ('k', 20)
  , ('L', 21)
  , ('l', 21)
  , ('M', 22)
  , ('m', 22)
  , ('N', 23)
  , ('n', 23)
  , ('O', 24)
  , ('o', 24)
  , ('P', 25)
  , ('p', 25)
  , ('Q', 26)
  , ('q', 26)
  , ('R', 27)
  , ('r', 27)
  , ('S', 28)
  , ('s', 28)
  , ('T', 29)
  , ('t', 29)
  , ('U', 30)
  , ('u', 30)
  , ('V', 31)
  , ('v', 31)
  , ('W', 32)
  , ('w', 32)
  , ('X', 33)
  , ('x', 33)
  , ('Y', 34)
  , ('y', 34)
  , ('Z', 35)
  , ('z', 35)
  , (' ', 36)
  , ('$', 37)
  , ('%', 38)
  , ('*', 39)
  , ('+', 40)
  , ('-', 41)
  , ('.', 42)
  , ('/', 43)
  , (':', 44)
  ]
