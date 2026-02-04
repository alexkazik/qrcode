{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Mode.Mixed
  ( mixed
  ) where

import           Codec.QRCode.Base

import qualified Data.DList                           as DL
import qualified Data.Map.Strict                      as M

import qualified Codec.QRCode.Data.ByteStreamBuilder  as BSB
import           Codec.QRCode.Data.QRSegment.Internal
import           Codec.QRCode.Data.Result
import           Codec.QRCode.Data.TextEncoding
import           Codec.QRCode.Data.ToInput
import           Codec.QRCode.Data.Version
import           Codec.QRCode.Mode.Alphanumeric
import           Codec.QRCode.Mode.Byte
import           Codec.QRCode.Mode.ECI
import           Codec.QRCode.Mode.Kanji
import           Codec.QRCode.Mode.Numeric

-- | Encode a string using any mode that seems fit, will encode the input in parts, each as
--   `numeric`, `alphanumeric`, `kanji` and `Codec.QRCode.Mode.Byte.text` based on the contents.
--
--   Please refer to the specific documentations for details.
--
--   Should result in the shortest encoded data.

mixed :: ToText a => TextEncoding -> a -> Result QRSegment
mixed te s =
  case s' of
    [] ->
      pure (constStream mempty)
    _ ->
      case te of
        Iso8859_1                 -> encIso1
        Utf8WithoutECI            -> encUtf8
        Utf8WithECI               -> encUtf8Eci
        Iso8859_1OrUtf8WithoutECI -> encIso1 <|> encUtf8
        Iso8859_1OrUtf8WithECI    -> encIso1 <|> encUtf8Eci
  where
    encIso1 :: Result QRSegment
    encIso1 = run EncISO1 <$> toIso1 ci s'
    encUtf8 :: Result QRSegment
    encUtf8 = run EncUtf8 <$> toUtf8 ci s'
    encUtf8Eci :: Result QRSegment
    encUtf8Eci = (<>) <$> eci 26 <*> encUtf8
    s' :: [Char]
    s' = toString s
    ci = isCI s

--
-- Internal types
--

data Type
  = TNumeric
  | TAlphanumeric
  | TKanji
  | T8Bit
  deriving (Eq)

data EightBitEncoding
  = EncUtf8
  | EncISO1
  deriving (Eq)

data Segment
  = S
    !Int -- Number of characters in the string
    !Int -- Number of bytes required to encode the string segment in `EightBitEncoding`
         -- (never used for Kanji characters when in `EncISO1` mode)
    !(DL.DList Char) -- String of the Segment

instance Semigroup Segment where
  {-# INLINE (<>) #-}
  (S i1 j1 s1) <> (S i2 j2 s2) = S (i1+i2) (j1+j2) (s1 `DL.append` s2)

type TypedSegment = (Type, Segment)

--
-- parse input
--

toIso1 :: Bool -> String -> Result [TypedSegment]
toIso1 ci = traverse toSeg
  where
    toSeg :: Char -> Result TypedSegment
    toSeg c =
      let
        tyc = typeOfChar ci c
        oc = ord c
      in
        if tyc == T8Bit && (oc < 0 || oc >= 256) -- in case of numeric, aplhanumeric or kanji it's already proven that it's a valid char
          then empty
          else pure (tyc, S 1 1 (DL.singleton c))

toUtf8 :: Bool -> String -> Result [TypedSegment]
toUtf8 ci = traverse toSeg
  where
    toSeg :: Char -> Result TypedSegment
    toSeg c =
      let
        tyc = typeOfChar ci c
        oc = ord c
      in
        case () of
          _ | oc <        0 -> empty
          _ | oc <     0x80 -> pure (tyc, S 1 1 (DL.singleton c))
          _ | oc <    0x800 -> pure (tyc, S 1 2 (DL.singleton c))
          _ | oc <  0x10000 -> pure (tyc, S 1 3 (DL.singleton c))
          _ | oc < 0x110000 -> pure (tyc, S 1 4 (DL.singleton c))
          _ | otherwise     -> empty

typeOfChar :: Bool -> Char -> Type
typeOfChar ci c
  | isDigit c = TNumeric
  | c `M.member` alphanumericMap ci = TAlphanumeric
  | c `M.member` kanjiMap = TKanji
  | otherwise = T8Bit

--
-- optimise segments and encode output
--

run :: EightBitEncoding -> [TypedSegment] -> QRSegment
run te sg = QRSegment $ \vr -> go vr sg'
  where
    go :: VersionRange -> [TypedSegment] -> Result BSB.ByteStreamBuilder
    go vr =
      fmap mconcat .
      traverse (encode te vr) .
      mergeTwo te vr .
      mergeMiddle 3 te vr .
      mergeMiddle 2 te vr .
      mergeMiddle 1 te vr
    sg' :: [TypedSegment]
    sg' = mergeEqual sg

--
-- encode output
--

encode :: EightBitEncoding -> VersionRange -> TypedSegment -> Result BSB.ByteStreamBuilder
encode te vr (ty, S i j s) =
  case (ty, te) of
    (TNumeric,      _) -> go 0b0001 i =<< numericB (DL.toList s)
    (TAlphanumeric, _) -> go 0b0010 i =<< alphanumericB True (DL.toList s)
    (T8Bit,   EncISO1) -> go 0b0100 j (BSB.fromList $ map (fromIntegral . ord) (DL.toList s))
    (T8Bit,   EncUtf8) -> go 0b0100 j =<< BSB.fromList <$> encodeUtf8 (DL.toList s)
    (TKanji,        _) -> go 0b1000 i =<< kanjiB (DL.toList s)
  where
    go :: Int -> Int -> BSB.ByteStreamBuilder -> Result BSB.ByteStreamBuilder
    go mode l sb
      | l < (1 `shiftL` pl) = pure (BSB.encodeBits 4 mode <> BSB.encodeBits pl l <> sb)
      | otherwise = empty
      where
        pl = pfxLen vr ty - 4 -- subtract the 4 bits for the mode from the length of the prefix

--
-- check sub/super relation between types
--

isSuper :: EightBitEncoding -> Type -> Type -> Bool
isSuper _       TAlphanumeric TNumeric      = True
isSuper _       T8Bit         TNumeric      = True
isSuper _       T8Bit         TAlphanumeric = True
isSuper EncUtf8 T8Bit         TKanji        = True
isSuper _       _             _             = False

commonSuper :: EightBitEncoding -> Type -> Type -> Maybe Type
commonSuper _     a       b
  | a == b = Just a
commonSuper _       TNumeric      TAlphanumeric = Just TAlphanumeric
commonSuper _       TAlphanumeric TNumeric      = Just TAlphanumeric
commonSuper _       TNumeric      T8Bit         = Just T8Bit
commonSuper _       T8Bit         TNumeric      = Just T8Bit
commonSuper _       TAlphanumeric T8Bit         = Just T8Bit
commonSuper _       T8Bit         TAlphanumeric = Just T8Bit
commonSuper EncUtf8 TKanji        _             = Just T8Bit
commonSuper EncUtf8 _             TKanji        = Just T8Bit
commonSuper _       _             _             = Nothing

--
-- calculate length of a TypedSegment
--

-- length of prefix (mode and length bits, depends on version range and type)

pfxLen :: VersionRange -> Type -> Int
pfxLen Version1to9   TNumeric      = 4 + 10
pfxLen Version10to26 TNumeric      = 4 + 12
pfxLen Version27to40 TNumeric      = 4 + 14
pfxLen Version1to9   TAlphanumeric = 4 +  9
pfxLen Version10to26 TAlphanumeric = 4 + 11
pfxLen Version27to40 TAlphanumeric = 4 + 13
pfxLen Version1to9   TKanji        = 4 +  8
pfxLen Version10to26 TKanji        = 4 + 10
pfxLen Version27to40 TKanji        = 4 + 12
pfxLen Version1to9   T8Bit         = 4 +  8
pfxLen Version10to26 T8Bit         = 4 + 16
pfxLen Version27to40 T8Bit         = 4 + 16

-- length of the data (depends on type)

encLen :: Type -> Segment -> Int
encLen TNumeric      (S i _ _) = let (j,k) = i `divMod` 3 in j * 10 + ([0,4,7] !! k)
encLen TAlphanumeric (S i _ _) = let (j,k) = i `divMod` 2 in j * 11 + k * 6
encLen TKanji        (S i _ _) = i * 13
encLen T8Bit         (S _ j _) = j * 8

-- length of a full segment (mode, length and data bits)

pfxEncLen :: VersionRange -> Type -> Segment -> Int
pfxEncLen vr ty g = pfxLen vr ty + encLen ty g

--
-- functions for merging segments
--

-- merge segments of equal type (not dependent on version range end encoding)

mergeEqual :: [TypedSegment] -> [TypedSegment]
mergeEqual ((t1, g1):(t2, g2):xs)
  | t1 == t2 = mergeEqual ((t1, g1<>g2):xs)
mergeEqual (x:xs) = x:mergeEqual xs
mergeEqual [] = []

-- merge tree neighboring segments (left, middle and right)

mergeMiddle :: Int -> EightBitEncoding -> VersionRange -> [TypedSegment] -> [TypedSegment]
mergeMiddle mt te vr = go
  where
    go (e1@(t1, g1):e2@(t2, g2):e3@(t3, g3):xs)
      -- (Phase 1-3) left and right are identical and are a super type of middle
      | t1 == t3 && isSuper te t1 t2 =
        if pfxEncLen vr t2 g2 + pfxLen vr t1 < encLen t1 g2
          then e1:go (e2:e3:xs)
          else go ((t1, g1<>g2<>g3):xs)
      -- (Phase 2-3) left and right are super of middle, left and right have a common super
      | mt >= 2 && isSuper te t1 t2 && isSuper te t3 t2 && isJust (commonSuper te t1 t3) =
        let
          g12 = g1 <> g2
          g23 = g2 <> g3
          g123 = g1 <> g2 <> g3
          tn = fromMaybe (error "commonSuper failed") $ commonSuper te t1 t3
          x1 = pfxEncLen vr t1 g12 + pfxEncLen vr t3 g3
          x2 = pfxEncLen vr t1 g1 + pfxEncLen vr t2 g2 + pfxEncLen vr t3 g3
          x3 = pfxEncLen vr t1 g1 + pfxEncLen vr t3 g23
          xn = pfxEncLen vr tn g123
        in
          if x2 <= x1 && x2 <= x3 && x2 < xn
            then e1:go (e2:e3:xs)
            else
              if xn <= x1 && xn <= x3
                then go ((tn, g123):xs)
                else
                  if x1 <= x3
                    then go ((t1,g12):e3:xs)
                    else go (e1:(t3,g23):xs)
      -- (Phase 2-3) left, middle and right have a common super
      | mt >= 2 && isJust (commonSuper te t2 =<< commonSuper te t1 t3) =
        let
          tn = fromMaybe (error "commonSuper failed") $ commonSuper te t2 =<< commonSuper te t1 t3
          x2 = pfxEncLen vr t1 g1 + pfxEncLen vr t2 g2 + pfxEncLen vr t3 g3
          g123 = g1 <> g2 <> g3
          xn = pfxEncLen vr tn g123
        in
          if x2 <= xn
            then e1:go (e2:e3:xs)
            else go ((tn, g123):xs)
      -- (Phase 3) left and right are super of middle
      | mt >= 3 && isSuper te t1 t2 && isSuper te t3 t2 =
        let
          x1 = encLen t1 g2
          x2 = pfxLen vr t2 + encLen t2 g2
          x3 = encLen t3 g2
        in
          if x2 <= x1 && x2 <= x3
            then e1:go (e2:e3:xs)
            else
              if x1 <= x3
                then go ((t1, g1<>g2):e3:xs)
                else go (e1:(t3, g2<>g3):xs)
    go (e1:xs) = e1 : go xs
    go [] = []

-- merge two neighboring segments when they use less space combined
mergeTwo :: EightBitEncoding -> VersionRange -> [TypedSegment] -> [TypedSegment]
mergeTwo te vr = go
  where
    go (e1@(t1,g1):e2@(t2,g2):xs) =
      case commonSuper te t1 t2 of
        Just t3 ->
          let
            x12 = pfxEncLen vr t1 g1 + pfxEncLen vr t2 g2
            g12 = g1<>g2
            x3 = pfxEncLen vr t3 g12
          in
            if x12 < x3
              then e1 : go (e2:xs)
              else go ((t3,g12):xs)
        Nothing -> e1 : go (e2:xs)
    go xs = xs
