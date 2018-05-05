{-# LANGUAGE NoImplicitPrelude #-}

module Codec.QRCode.Data.TextEncoding
  ( TextEncoding(..)
  ) where

data TextEncoding
  = Iso8859_1
    -- ^ Generate segment out of a ISO-8859-1 text, if the text contains chars
    --   which aren't in the charset the result will be a failure.
  | Utf8WithoutECI
    -- ^ Use an UTF-8 encoded text.
    --   The reader must do a detection and conversion.
    --
    --   __Please check the readers which should be used if they support this.__
  | Utf8WithECI
    -- ^ This is the correct way to encode UTF-8, but it's reported that not all
    --   readers support this.
    --
    --   __Please check the readers which should be used if they support this.__
  | Iso8859_1OrUtf8WithoutECI
    -- ^ Try to encode as `Iso8859_1`, if that is not possible use `Utf8WithoutECI`.
  | Iso8859_1OrUtf8WithECI
    -- ^ Try to encode as `Iso8859_1`, if that is not possible use `Utf8WithECI`.
