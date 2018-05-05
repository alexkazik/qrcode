{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}

#ifndef MIN_VERSION_semigroups
#define MIN_VERSION_semigroups(a,b,c) 0
#endif

module Codec.QRCode.Data.Result
  ( Result(..)
  , result
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Data.Data
import           Data.Semigroup
import           GHC.Generics
#if !(MIN_VERSION_base(4,8,0))
import           Data.Foldable
import           Data.Traversable
#endif
#if (MIN_VERSION_base(4,9,0))
import           Control.Monad.Fail
import           Control.Monad.Zip
import           Data.Functor.Classes
#endif

-- | This Maybe wrapper treats Just as success and Nothing as failure.
--   There is no unexpected success \<-> failure change.
--
--   Differences:
--
--   @
--     instance Semigroup/Monoid Maybe where
--        Nothing <> (Just x) = Just x
--        mempty = Nothing
--        stimes 0 (Just x) = Nothing
--
--     instance Semigroup/Monoid Result where
--        Nothing <> (Just x) = Nothing
--        mempty = Just mempty
--        stimes 0 (Just x) = Just (stimes 0 x)
--   @
newtype Result a
  = Result
    { getResult :: Maybe a
    }
  deriving
    ( Alternative, Applicative, Data, Eq, Foldable, Functor
    , Generic, Generic1, Monad, MonadFix, MonadPlus
    , Ord, Read, Show, Traversable, Typeable
#if (MIN_VERSION_base(4,9,0))
    , Eq1, MonadFail, MonadZip, Ord1, Read1, Show1
#endif
    )

instance Semigroup a => Semigroup (Result a) where
  (Result (Just a)) <> (Result (Just b)) = Result (Just (a <> b))
  _ <> _ = Result Nothing
#if (MIN_VERSION_semigroups(0,17,0)) || (MIN_VERSION_base(4,9,0))
  stimes n (Result (Just a)) = Result (Just (stimes n a))
  stimes _ _                 = Result Nothing
#endif

instance Monoid a => Monoid (Result a) where
  {-# INLINABLE mempty #-}
  mempty = Result (Just mempty)
#if !(MIN_VERSION_base(4,11,0))
  (Result (Just a)) `mappend` (Result (Just b)) = Result (Just (a `mappend` b))
  _ `mappend` _ = Result Nothing
#endif

result :: b -> (a -> b) -> Result a -> b
{-# INLINE result #-}
result n j (Result m) = maybe n j m
