-- | Export everything needed from base in addition to the Prelude

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Base
  ( module X
  ) where

import           Control.Applicative as X (Alternative (..))
import           Control.Monad       as X (forM, guard, unless, void, when)
import           Control.Monad.ST    as X (runST)
import           Data.Bits           as X (Bits (..))
import           Data.Bool           as X (bool)
import           Data.Char           as X (isDigit, ord, toUpper)
import           Data.Foldable       as X (foldl', foldlM, forM_)
import           Data.List           as X (mapAccumL, sortOn, transpose)
import           Data.Maybe          as X (fromMaybe, isJust)
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup      as X (Semigroup (..))
#endif
import           Data.STRef          as X (modifySTRef', newSTRef, readSTRef)
import           Data.Tuple          as X (swap)
import           Data.Word           as X (Word16, Word8)
import           GHC.Exts            as X (IsList (Item, fromListN))
import           Prelude             as X
